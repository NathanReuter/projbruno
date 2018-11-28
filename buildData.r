stringpath = "~/Downloads/saved rds/";
folders = list.files(stringpath);

allData = data.frame();

for (subf in folders) {
  folderPath = paste(stringpath, subf, sep = "");
  files = list.files(folderPath);
  for (file in files) {
    data = readRDS(paste(folderPath, file, sep = "/"));
    allData = rbind(allData, data);
  }
}

dupl = duplicated(allData[, c(2)]);
allData = allData[!dupl, ];
saveRDS(allData, "./data/allData.rds")