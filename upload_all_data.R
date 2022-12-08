
# data cor 200 dpi
id_cor_200 <- do.call(rbind, idList) 
id_cor_200 <- as.data.frame(id_cor_200) 
id_cor_200[,1] <- factor(id_cor_200[,1])
round(prop.table(table(id_cor_200$V1)) * 100, digits = 1)

id_cor_sn_200 <- as.data.frame(lapply(id_cor_200[-1], normalize))

# data cor 100 dpi
id_cor_100 <- do.call(rbind, idList) 
id_cor_100 <- as.data.frame(id_cor_100) 
id_cor_100[,1] <- factor(id_cor_100[,1])
round(prop.table(table(id_cor_100$V1)) * 100, digits = 1)

id_cor_sn_100 <- as.data.frame(lapply(id_cor_100[-1], normalize))

# data cor 300 dpi
id_cor_300 <- do.call(rbind, idList) 
id_cor_300 <- as.data.frame(id_cor_300) 
id_cor_300[,1] <- factor(id_cor_300[,1])
round(prop.table(table(id_cor_300$V1)) * 100, digits = 1)

id_cor_sn_300 <- as.data.frame(lapply(id_cor_300[-1], normalize))

# data pre 200 dpi
id_pre_200 <- do.call(rbind, idList) 
id_pre_200 <- as.data.frame(id_pre_200) 
id_pre_200[,1] <- factor(id_pre_200[,1])
round(prop.table(table(id_pre_200$V1)) * 100, digits = 1)

id_pre_sn_200 <- as.data.frame(lapply(id_pre_200[-1], normalize))
# data pre 300 dpi
id_pre_300 <- do.call(rbind, idList) 
id_pre_300 <- as.data.frame(id_pre_300) 
id_pre_300[,1] <- factor(id_pre_300[,1])
round(prop.table(table(id_pre_300$V1)) * 100, digits = 1)

id_pre_sn_300 <- as.data.frame(lapply(id_pre_300[-1], normalize))


id_pre_sn_pca_300 <- prcomp(id_cor_sn_300,center = TRUE,scale. = TRUE)
id_cor_sn_pca_100 <- prcomp(id_cor_sn_100,center = TRUE,scale. = TRUE)
id_cor_sn_pca_200 <- prcomp(id_cor_sn_200,center = TRUE,scale. = TRUE)

