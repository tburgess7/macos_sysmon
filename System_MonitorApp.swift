import SwiftUI
import Combine
import Foundation
import Darwin.Mach   // For host_processor_info, vm_deallocate, etc.
import Darwin        // For getifaddrs, freeifaddrs, etc.


// MARK: - Helper Structures

struct Sample: Identifiable {
    let id = UUID()
    let timestamp: Date
    let value: Double
}

struct NetworkStat: Identifiable, Equatable {
    let id = UUID()
    let interface: String
    let rxBps: UInt64
    let txBps: UInt64

    static func == (lhs: NetworkStat, rhs: NetworkStat) -> Bool {
        return lhs.interface == rhs.interface &&
               lhs.rxBps == rhs.rxBps &&
               lhs.txBps == rhs.txBps
    }
}


// MARK: - CPUUsageHelper

class CPUUsageHelper {
    private var previousInfo: [Int32]?
    private var previousInfoCount: mach_msg_type_number_t = 0
    private var previousNumCPU: natural_t = 0

    func updateUsage() -> (overall: Double, perCore: [Double])? {
        var kr: kern_return_t
        var numCPU: natural_t = 0
        var cpuInfo: processor_info_array_t?
        var infoCount: mach_msg_type_number_t = 0

        kr = host_processor_info(mach_host_self(), PROCESSOR_CPU_LOAD_INFO, &numCPU, &cpuInfo, &infoCount)
        if kr != KERN_SUCCESS { return nil }
        let numCPUs = Int(numCPU)
        var perCoreUsages = [Double](repeating: 0.0, count: numCPUs)
        var overallUsage: Double = 0.0

        if let prev = previousInfo, let cpuInfo = cpuInfo {
            let cpuInfoArray = Array(UnsafeBufferPointer(start: cpuInfo, count: Int(infoCount)))
            for i in 0..<numCPUs {
                let base = i * Int(CPU_STATE_MAX)
                let user   = Double(cpuInfoArray[base + Int(CPU_STATE_USER)] - prev[base + Int(CPU_STATE_USER)])
                let system = Double(cpuInfoArray[base + Int(CPU_STATE_SYSTEM)] - prev[base + Int(CPU_STATE_SYSTEM)])
                let nice   = Double(cpuInfoArray[base + Int(CPU_STATE_NICE)] - prev[base + Int(CPU_STATE_NICE)])
                let idle   = Double(cpuInfoArray[base + Int(CPU_STATE_IDLE)] - prev[base + Int(CPU_STATE_IDLE)])
                let totalTicks = user + system + nice + idle
                let busyTicks = user + system + nice
                let coreUsage = totalTicks > 0 ? busyTicks / totalTicks * 100.0 : 0
                perCoreUsages[i] = coreUsage
                overallUsage += coreUsage
            }
            overallUsage = overallUsage / Double(numCPUs)
        } else {
            perCoreUsages = [Double](repeating: 0.0, count: numCPUs)
            overallUsage = 0.0
        }

        if let cpuInfo = cpuInfo {
            let buffer = UnsafeBufferPointer(start: cpuInfo, count: Int(infoCount))
            self.previousInfo = Array(buffer)
            self.previousNumCPU = numCPU
            self.previousInfoCount = infoCount
            let cpuInfoSize = Int(infoCount) * MemoryLayout<integer_t>.size
            vm_deallocate(mach_task_self_, vm_address_t(bitPattern: cpuInfo), vm_size_t(cpuInfoSize))
        }
        return (overall: overallUsage, perCore: perCoreUsages)
    }
}


// MARK: - SystemMonitor

class SystemMonitor: ObservableObject {
    private let maxHistory = 30

    // CPU properties.
    @Published var cpuUsage: Double = 0.0
    @Published var cpuHistory: [Sample] = []
    @Published var cpuCoreUsages: [Double] = []
    @Published var cpuCoreHistory: [[Sample]] = []

    // Memory properties.
    @Published var memoryUsed: UInt64 = 0
    @Published var memoryFree: UInt64 = 0
    @Published var memoryTotal: UInt64 = 0
    @Published var memoryHistory: [Sample] = []

    // Disk properties.
    @Published var diskUsed: UInt64 = 0
    @Published var diskFree: UInt64 = 0
    @Published var diskTotal: UInt64 = 0
    @Published var diskHistory: [Sample] = []

    // Network properties.
    @Published var networkStats: [NetworkStat] = []
    @Published var networkHistory: [String: [NetworkStat]] = [:]

    private var timer: Timer?
    private let cpuHelper = CPUUsageHelper()
    private var previousNetworkCounters: [String: (rx: UInt64, tx: UInt64)] = [:]
    private var lastNetworkUpdate: Date = Date()

    func startMonitoring() {
        updateMetrics()
        timer = Timer.scheduledTimer(withTimeInterval: 1.0, repeats: true) { _ in
            self.updateMetrics()
        }
    }

    func stopMonitoring() {
        timer?.invalidate()
        timer = nil
    }

    private func updateMetrics() {
        updateCPU()
        updateMemory()
        updateDisk()
        updateNetwork()
    }

    private func updateCPU() {
        if let usageData = cpuHelper.updateUsage() {
            DispatchQueue.main.async {
                self.cpuUsage = usageData.overall
                self.cpuHistory.append(Sample(timestamp: Date(), value: usageData.overall))
                if self.cpuHistory.count > self.maxHistory { self.cpuHistory.removeFirst() }
                self.cpuCoreUsages = usageData.perCore
                if self.cpuCoreHistory.count != usageData.perCore.count {
                    self.cpuCoreHistory = Array(repeating: [], count: usageData.perCore.count)
                }
                for i in 0..<usageData.perCore.count {
                    self.cpuCoreHistory[i].append(Sample(timestamp: Date(), value: usageData.perCore[i]))
                    if self.cpuCoreHistory[i].count > self.maxHistory { self.cpuCoreHistory[i].removeFirst() }
                }
            }
        }
    }

    private func updateMemory() {
        let total = ProcessInfo.processInfo.physicalMemory
        var vmStats = vm_statistics64()
        var count = mach_msg_type_number_t(MemoryLayout<vm_statistics64_data_t>.size / MemoryLayout<integer_t>.size)
        let result = withUnsafeMutablePointer(to: &vmStats) {
            $0.withMemoryRebound(to: integer_t.self, capacity: Int(count)) {
                host_statistics64(mach_host_self(), HOST_VM_INFO64, $0, &count)
            }
        }
        if result == KERN_SUCCESS {
            var pageSize: vm_size_t = 0
            host_page_size(mach_host_self(), &pageSize)
            let free = UInt64(vmStats.free_count + vmStats.inactive_count) * UInt64(pageSize)
            let used = total > free ? total - free : 0
            let memPercent = total > 0 ? (Double(used) / Double(total)) * 100.0 : 0.0
            DispatchQueue.main.async {
                self.memoryFree = free
                self.memoryUsed = used
                self.memoryTotal = total
                self.memoryHistory.append(Sample(timestamp: Date(), value: memPercent))
                if self.memoryHistory.count > self.maxHistory { self.memoryHistory.removeFirst() }
            }
        }
    }

    private func updateDisk() {
        do {
            let attrs = try FileManager.default.attributesOfFileSystem(forPath: "/")
            if let totalSpace = attrs[.systemSize] as? NSNumber,
               let freeSpace = attrs[.systemFreeSize] as? NSNumber {
                let total = totalSpace.uint64Value
                let free = freeSpace.uint64Value
                let used = total > free ? total - free : 0
                let diskPercent = total > 0 ? (Double(used) / Double(total)) * 100.0 : 0.0
                DispatchQueue.main.async {
                    self.diskTotal = total
                    self.diskFree = free
                    self.diskUsed = used
                    self.diskHistory.append(Sample(timestamp: Date(), value: diskPercent))
                    if self.diskHistory.count > self.maxHistory { self.diskHistory.removeFirst() }
                }
            }
        } catch {
            print("Error retrieving disk info: \(error)")
        }
    }

    private func updateNetwork() {
        DispatchQueue.global(qos: .background).async {
            let currentCounters = self.getNetworkCounters()
            let now = Date()
            let interval = now.timeIntervalSince(self.lastNetworkUpdate)
            self.lastNetworkUpdate = now

            var stats: [NetworkStat] = []
            for (iface, counters) in currentCounters {
                let previous = self.previousNetworkCounters[iface] ?? (rx: counters.rx, tx: counters.tx)
                let rxDelta = counters.rx >= previous.rx ? counters.rx - previous.rx : 0
                let txDelta = counters.tx >= previous.tx ? counters.tx - previous.tx : 0
                let rxBps = UInt64(Double(rxDelta) / interval)
                let txBps = UInt64(Double(txDelta) / interval)
                if rxBps > 0 || txBps > 0 {
                    stats.append(NetworkStat(interface: iface, rxBps: rxBps, txBps: txBps))
                }
            }
            self.previousNetworkCounters = currentCounters
            DispatchQueue.main.async {
                self.networkStats = stats
                var interfaces = Set(self.networkHistory.keys)
                for stat in stats { interfaces.insert(stat.interface) }
                for iface in interfaces {
                    let sample: NetworkStat
                    if let stat = stats.first(where: { $0.interface == iface }) {
                        sample = stat
                    } else {
                        sample = NetworkStat(interface: iface, rxBps: 0, txBps: 0)
                    }
                    if self.networkHistory[iface] == nil { self.networkHistory[iface] = [] }
                    self.networkHistory[iface]?.append(sample)
                    if self.networkHistory[iface]!.count > self.maxHistory {
                        self.networkHistory[iface]?.removeFirst()
                    }
                }
            }
        }
    }

    private func getNetworkCounters() -> [String: (rx: UInt64, tx: UInt64)] {
        var counters: [String: (rx: UInt64, tx: UInt64)] = [:]
        var ifaddrPtr: UnsafeMutablePointer<ifaddrs>?
        if getifaddrs(&ifaddrPtr) == 0 {
            var ptr = ifaddrPtr
            while ptr != nil {
                if let interface = ptr?.pointee {
                    let name = String(cString: interface.ifa_name)
                    if let data = interface.ifa_data, name != "lo0" {
                        let networkData = data.assumingMemoryBound(to: if_data.self).pointee
                        let rxBytes = UInt64(networkData.ifi_ibytes)
                        let txBytes = UInt64(networkData.ifi_obytes)
                        if let existing = counters[name] {
                            counters[name] = (rx: existing.rx + rxBytes, tx: existing.tx + txBytes)
                        } else {
                            counters[name] = (rx: rxBytes, tx: txBytes)
                        }
                    }
                }
                ptr = ptr?.pointee.ifa_next
            }
            freeifaddrs(ifaddrPtr)
        }
        return counters
    }

    static func humanReadableBytes(_ bytes: UInt64) -> String {
        let formatter = ByteCountFormatter()
        // Use .memory style for binary units (GiB, MiB, etc.)
        formatter.countStyle = .memory
        return formatter.string(fromByteCount: Int64(bytes))
    }
}


// MARK: - BarGraph View

struct BarGraph: View {
    let samples: [Double]
    let fixedMax: Double?
    let color: Color

    var computedMax: Double {
        fixedMax ?? (samples.max() ?? 1)
    }

    var body: some View {
        GeometryReader { geometry in
            let availableWidth = geometry.size.width
            let availableHeight = geometry.size.height
            let count = samples.count
            let spacing: CGFloat = 1
            let totalSpacing = spacing * CGFloat(max(count - 1, 0))
            let barWidth = count > 0 ? (availableWidth - totalSpacing) / CGFloat(count) : 0

            HStack(alignment: .bottom, spacing: spacing) {
                ForEach(0..<count, id: \.self) { index in
                    Rectangle()
                        .fill(color)
                        .frame(width: barWidth,
                               height: computedMax > 0 ? CGFloat(samples[index] / computedMax) * availableHeight : 0)
                }
            }
            .frame(width: availableWidth, height: availableHeight, alignment: .bottomLeading)
            .background(Color.gray.opacity(0.2))
            .cornerRadius(4)
            .clipped()
        }
    }
}


// MARK: - HeaderGroup View

struct HeaderGroup<Content: View>: View {
    let header: String
    let content: Content

    init(header: String, @ViewBuilder content: () -> Content) {
         self.header = header
         self.content = content()
    }

    var body: some View {
         VStack(alignment: .leading, spacing: 4) {
              Text(header)
                   .font(.headline)
                   .padding(.top, 15)
                   .padding(.leading, 8)
              Divider()
              content
                   .frame(maxWidth: .infinity, alignment: .leading)
         }
         .background(RoundedRectangle(cornerRadius: 8).stroke(Color.gray))
    }
}


struct ContentView: View {
    @ObservedObject var monitor = SystemMonitor()

    private func aggregateNetworkHistory(for keyPath: KeyPath<NetworkStat, UInt64>, from history: [String: [NetworkStat]]) -> [Double] {
         let count = history.values.map { $0.count }.max() ?? 0
         var aggregated: [Double] = []
         for i in 0..<count {
              var sum: UInt64 = 0
              for (_, samples) in history {
                   if i < samples.count {
                        sum += samples[i][keyPath: keyPath]
                   }
              }
              aggregated.append(Double(sum))
         }
         return aggregated
    }

    var body: some View {
         GeometryReader { geometry in
              ScrollView {
                   VStack(alignment: .leading, spacing: 16) {
                        
                        // Top row for Overall CPU Usage, Memory, and Disk.
                        HStack(alignment: .top, spacing: 16) {
                             HeaderGroup(header: "Overall CPU Usage") {
                                  VStack(alignment: .leading, spacing: 8) {
                                       Text(String(format: "Usage: %.2f%%", monitor.cpuUsage))
                                            .font(Font.system(.caption, design: .monospaced))
                                       BarGraph(samples: monitor.cpuHistory.map { $0.value },
                                                fixedMax: 100,
                                                color: .green)
                                            .frame(height: 120)
                                  }
                                  .padding(8)
                             }
                             .frame(maxWidth: .infinity)
                             
                             HeaderGroup(header: "Memory") {
                                  VStack(alignment: .leading, spacing: 8) {
                                      Text("Used: \(SystemMonitor.humanReadableBytes(monitor.memoryUsed))")
                                            .font(Font.system(.caption, design: .monospaced))
                                      BarGraph(samples: monitor.memoryHistory.map { $0.value },
                                                fixedMax: 100,
                                                color: .orange)
                                            .frame(height: 120)
                                  }
                                  .padding(8)
                             }
                             .frame(maxWidth: .infinity)
                             
                             HeaderGroup(header: "Disk") {
                                  VStack(alignment: .leading, spacing: 8) {
                                      Text("Used: \(SystemMonitor.humanReadableBytes(monitor.diskUsed))")
                                            .font(Font.system(.caption, design: .monospaced))
                                      BarGraph(samples: monitor.diskHistory.map { $0.value },
                                                fixedMax: 100,
                                                color: .pink)
                                            .frame(height: 120)
                                  }
                                  .padding(8)
                             }
                             .frame(maxWidth: .infinity)
                        }
                        
                        // CPU Per Core Section
                        HeaderGroup(header: "CPU Per Core") {
                             ScrollView(.horizontal, showsIndicators: false) {
                                  HStack(spacing: 12) {
                                        ForEach(monitor.cpuCoreHistory.indices, id: \.self) { i in
                                             VStack(alignment: .center, spacing: 4) {
                                                  Text("\(i) - \(monitor.cpuCoreHistory[i].last?.value ?? 0, specifier: "%.2f")%")
                                                      .font(Font.system(size: 12, design: .monospaced))
                                                  BarGraph(samples: monitor.cpuCoreHistory[i].map { $0.value },
                                                           fixedMax: 100,
                                                           color: .purple)
                                                      .frame(width: 100, height: 100)
                                             }
                                             .frame(width: 100)
                                        }
                                  }
                                  .padding(.horizontal, 8)
                             }
                        }
                        
                        // Network Traffic Section.
                        HeaderGroup(header: "Network Traffic") {
                             VStack(alignment: .leading, spacing: 8) {
                                  Text("Download: \(SystemMonitor.humanReadableBytes(monitor.networkStats.reduce(0) { $0 + $1.rxBps }))/s")
                                       .font(.caption)
                                  let aggregatedRx = aggregateNetworkHistory(for: \.rxBps, from: monitor.networkHistory)
                                  BarGraph(samples: aggregatedRx, fixedMax: nil, color: .blue)
                                       .frame(height: 80)
                                  Text("Upload: \(SystemMonitor.humanReadableBytes(monitor.networkStats.reduce(0) { $0 + $1.txBps }))/s")
                                       .font(.caption)
                                  let aggregatedTx = aggregateNetworkHistory(for: \.txBps, from: monitor.networkHistory)
                                  BarGraph(samples: aggregatedTx, fixedMax: nil, color: .blue)
                                       .frame(height: 80)
                             }
                             .padding(8)
                        }
                   }
                   .padding()
                   .frame(minWidth: geometry.size.width)
              }
         }
         .onAppear {
              monitor.startMonitoring()
         }
         .onDisappear {
              monitor.stopMonitoring()
         }
    }
}



@main
struct SystemMonitorApp: App {
    var body: some Scene {
         WindowGroup {
              ContentView()
         }
    }
}
