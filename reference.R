reference = read.xlsx("C:/Users/WXY/Documents/学校/thesis/毕业论文/reference.xlsx")

gsub("\\[[0-9]+\\]", "", reference$reference[9])

reference$reference = gsub("\\[[0-9]+\\]|\\[\\]", "", reference$reference)
reference$reference = trimws(reference$reference)
reference$reference = sort(reference$reference)

reference = read.xlsx("data/reference.xlsx")

reference$reference = paste0("【", 1:nrow(reference), "】. ", reference$reference)

write.xlsx(reference, "data/reference.xlsx")
