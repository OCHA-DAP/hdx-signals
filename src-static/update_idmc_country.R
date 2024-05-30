box::use(logger[log_info])

box::use(cs = ../src/utils/cloud_storage)
box::use(../src/utils/hs_logger)

hs_logger$configure_logger()

log_info("Updating hard-coded IDMC info...")

# TODO: Reference source
# nolint start
idmc_country_links <- structure(list(iso3 = c("AB9", "AFG", "AGO", "ALB", "ARG", "ARM",
"ATG", "AUS", "AUT", "AZE", "BDI", "BEN", "BFA", "BGD", "BGR",
"BHS", "BIH", "BLR", "BLZ", "BOL", "BRA", "BRN", "BTN", "BWA",
"CAF", "CAN", "CHE", "CHL", "CHN", "CIV", "CMR", "COD", "COG",
"COK", "COL", "COM", "CPV", "CRI", "CUB", "CYM", "CYP", "CZE",
"DEU", "DJI", "DMA", "DOM", "DZA", "ECU", "EGY", "ERI", "ESP",
"ETH", "FJI", "FIN", "FRA", "FSM", "GAB", "GBR", "GEO", "GHA",
"GIN", "GMB", "GNB", "GRC", "GTM", "HND", "HRV", "HTI", "HUN",
"IDN", "IND", "IRL", "IRN", "IRQ", "ISR", "ITA", "JAM", "JOR",
"JPN", "KAZ", "KEN", "KGZ", "KHM", "KIR", "KOR", "KOS", "LAO",
"LBN", "LBR", "LBY", "LCA", "LKA", "LSO", "MAR", "MDA", "MDG",
"MEX", "MHL", "MKD", "MLI", "MMR", "MNE", "MNG", "MOZ", "MRT",
"MUS", "MWI", "MYS", "MYT", "NAM", "NCL", "NER", "NGA", "NIC",
"NLD", "NOR", "NPL", "NZL", "OMN", "PAK", "PAN", "PER", "PHL",
"PLW", "PNG", "POL", "PRK", "PRT", "PRY", "PSE", "PYF", "ROU",
"RUS", "RWA", "SAU", "SDN", "SEN", "SLB", "SLE", "SLV", "SOM",
"SRB", "SSD", "SUR", "SWE", "SWZ", "SYC", "SYR", "TCD", "TGO",
"THA", "TLS", "TJK", "TKM", "TON", "TTO", "TUN", "TUR", "TUV",
"TWN", "TZA", "UGA", "UKR", "URY", "USA", "UZB", "VCT", "VEN",
"VNM", "VUT", "WSM", "YEM", "ZAF", "ZMB", "ZWE"), country_link = c("abyei",
"afghanistan", "angola", "albania", "argentina", "armenia", "antigua-and-barbuda",
"australia", "austria", "azerbaijan", "burundi", "benin", "burkina-faso",
"bangladesh", "bulgaria", "bahamas", "bosnia-and-herzegovina",
"belarus", "belize", "bolivia", "brazil", "brunei-darussalam",
"bhutan", "botswana", "central-african-republic", "canada", "switzerland",
"chile", "china", "cote-divoire", "cameroon", "democratic-republic-of-the-congo",
"congo", "cook-islands", "colombia", "comoros", "cabo-verde",
"costa-rica", "cuba", "cayman-islands", "cyprus", "czech-republic",
"germany", "djibouti", "dominica", "dominican-republic", "algeria",
"ecuador", "egypt", "eritrea", "spain", "ethiopia", "fiji", "finland",
"france", "micronesia", "gabon", "united-kingdom", "georgia",
"ghana", "guinea", "gambia", "guinea-bissau", "greece", "guatemala",
"honduras", "croatia", "haiti", "hungary", "indonesia", "india",
"ireland", "iran", "iraq", "israel", "italy", "jamaica", "jordan",
"japan", "kazakhstan", "kenya", "kyrgyzstan", "cambodia", "kiribati",
"republic-of-korea", "kosovo", "laos", "lebanon", "liberia",
"libya", "saint-lucia", "sri-lanka", "lesotho", "morocco", "moldova",
"madagascar", "mexico", "marshall-islands", "north-macedonia",
"mali", "myanmar", "montenegro", "mongolia", "mozambique", "mauritania",
"mauritius", "malawi", "malaysia", "mayotte", "namibia", "new-caledonia",
"niger", "nigeria", "nicaragua", "netherlands", "norway", "nepal",
"new-zealand", "oman", "pakistan", "panama", "peru", "philippines",
"palau", "papua-new-guinea", "poland", "democratic-peoples-republic-of-korea",
"portugal", "paraguay", "palestine", "french-polynesia", "romania",
"russia", "rwanda", "saudi-arabia", "sudan", "senegal", "solomon-islands",
"sierra-leone", "el-salvador", "somalia", "serbia", "south-sudan",
"suriname", "sweden", "eswatini", "seychelles", "syria", "chad",
"togo", "thailand", "timor-leste", "tajikistan", "turkmenistan",
"tonga", "trinidad-and-tobago", "tunisia", "turkiye", "tuvalu",
"taiwan", "tanzania", "uganda", "ukraine", "uruguay", "united-states",
"uzbekistan", "saint-vincent-and-the-grenadines", "venezuela",
"viet-nam", "vanuatu", "samoa", "yemen", "south-africa", "zambia",
"zimbabwe")), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA,
-175L))
# nolint end

fname <- "input/idmc_country_links.parquet"
cs$update_az_file(
  df = idmc_country_links,
  name = fname
)

log_info(paste0("Successfully saved location links to ", fname))
