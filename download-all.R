versions <- ihpdr::ihpd_versions()
source("create-excel-fun.R")
# for (ver in versions[9:30]) {
#   download_version(ver)
# }

vers = "1102"
tmplt_xlsx = "hp1102-1104.xlsx"

download_version("1102", "hp1102-1104.xlsx")
download_version("1103", "hp1102-1104.xlsx")
download_version("1104", "hp1102-1104.xlsx")

vers = "1302"
tmplt_xlsx = "hpta/hpta1302.xlsx"
download_version("1302", "hpta/hpta1302.xlsx")
download_version("1303", "hpta/hpta1302.xlsx")
download_version("1304", "hpta/hpta1304.xlsx")

download_version("1401", "hpta/hpta1401.xlsx")
download_version("1402", "hpta/hpta1402.xlsx")
download_version("1403", "hpta/hpta1403.xlsx")
download_version("1404", "hpta/hpta1404.xlsx")

download_version("1501", "hpta/hpta1501.xlsx")
download_version("1502", "hpta/hpta1502.xlsx")
download_version("1503", "hpta/hpta1503.xlsx")
download_version("1504", "hpta/hpta1504.xlsx")

download_version("1601", "hpta/hpta1601.xlsx")
download_version("1602", "hpta/hpta1602.xlsx")
download_version("1603", "hpta/hpta1603.xlsx")
download_version("1604", "hpta/hpta1604.xlsx")

download_version("1701", "hpta/hpta1701.xlsx")
download_version("1702", "hpta/hpta1702.xlsx")
download_version("1703", "hpta/hpta1703.xlsx")
download_version("1704", "hpta/hpta1704.xlsx")

download_version("1801", "hpta/hpta1801.xlsx")
download_version("1802", "hpta/hpta1802.xlsx")
download_version("1803", "hpta/hpta1803.xlsx")
download_version("1804", "hpta/hpta1804.xlsx")

download_version("1901", "hpta/hpta1901.xlsx")
download_version("1902", "hpta/hpta1902.xlsx")
download_version("1903", "hpta/hpta1903.xlsx")
download_version("1904", "hpta/hpta1904.xlsx")

vers = "2001"
tmplt_xlsx = "full_post2001.xlsx"

download_version("2001", "full_post2001.xlsx")
download_version("2002", "full_post2002.xlsx")
download_version("2003", "full_post2002.xlsx")
download_version("2004", "full_post2002.xlsx")

download_version("2101", "full_post2002.xlsx")
download_version("2102", "full_post2002.xlsx")
download_version("2103", "full_post2002.xlsx")
download_version("2104", "full_post2002.xlsx")

download_version("2201", "full_post2002.xlsx")
