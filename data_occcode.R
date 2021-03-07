occup_countryedu90 = read.xlsx('data/occupation_province.xlsx', sheet = "1990_edu")

# occ_code90cn = data.frame(label = occup_countryedu90$occupation[2:379], stringsAsFactors = F)
# occ_code90cn$class2[!grepl("\\s\\s\\s", occ_code90cn$label)&grepl("\\s\\s", occ_code90cn$label)] = occ_code90cn$label[!grepl("\\s\\s\\s", occ_code90cn$label)&grepl("\\s\\s", occ_code90cn$label)]
# occ_code90cn$class1[!grepl("\\s", occ_code90cn$label)] = occ_code90cn$label[!grepl("\\s", occ_code90cn$label)]
# occ_code90cn$class2 = zoo::na.locf(occ_code90cn$class2, na.rm = FALSE)
# occ_code90cn$class1 = zoo::na.locf(occ_code90cn$class1, na.rm = FALSE)
# occ_code90cn = subset(occ_code90cn, class2 != label & class1 != label)
# occ_code90cn$class2 = gsub(" ", "", occ_code90cn$class2)
# occ_code90cn$label = gsub(" ", "", occ_code90cn$label)
# xlsx::write.xlsx(occ_code90cn, "data/occupation_code.xlsx", append = TRUE, sheetName = "hahaha")
