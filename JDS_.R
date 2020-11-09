library(readxl)
data=read_xlsx("E:/Data_Nilai_Pegawai.xlsx")
View(data)
library(ggplot2)
library(RColorBrewer)
library(doBy)

summary(data)

#variabel Tingkat Pendidikan Akhir
counts=table(data$`Tingkat Pendidikan Akhir`)
barplot(counts,main="Tingkat Pendidikan Akhir Pegawai",
        xlab="pendidikan akhir", ylab="jumlah",col=brewer.pal(8,"Set3"))

mytable=table(data$`Tingkat Pendidikan Akhir`)
pct=((mytable/sum(mytable))*100)
pct=round(pct,digit=2)
lbls=paste(names(mytable),pct)
lbls=paste(lbls,"%",sep="")
pie(mytable,labels = lbls,
    main="Pie Chart dari Tingkat Pendidikan Akhir Pegawai")

boxplot(data$Nilai~data$`Tingkat Pendidikan Akhir`,
        data=data,main="Perbedaan Boxplot untuk Masing-Masing Tingkat Pendidikan Akhir",
        xlab="Tingkat Pendidikan Akhir",
        ylab="Nilai", col="orange",border="brown")


#variabel Satuan Kerja
counts=table(data$`Satuan Kerja`)
barplot(counts,main="Satuan Kerja",
      ylab="jumlah",col=brewer.pal(6,"Set2"))

mytable=table(data$`Satuan Kerja`)
pct=((mytable/sum(mytable))*100)
pct=round(pct,digit=2)
lbls=paste(names(mytable),pct)
lbls=paste(lbls,"%",sep="")
pie(mytable,labels = lbls,
    col=rainbow(length(lbls)),
    main="Pie Chart dari Satuan Kerja Pegawai")

boxplot(data$Nilai~data$`Satuan Kerja`,
        data=data,main="Perbedaan Boxplot untuk Masing-Masing Satuan Kerja",
        xlab="Satuan Kerja",
        ylab="Nilai", col="orange",border="brown")

#variabel Usia
plot(data$Usia,data$Nilai,main="Persebaran Nilai Berdasarkan Usia",ylab="Nilai",xlab="Usia",col=brewer.pal(3,"Set2"))

#ANOVA AND TUKEY TEST
Anova_result=aov(data$Nilai~data$`Tingkat Pendidikan Akhir`,data=data)
summary(Anova_result)
tukey=TukeyHSD(Anova_result)
tukey
plot(tukey)

Anova_result=aov(data$Nilai~data$`Satuan Kerja`,data=data)
summary(Anova_result)
tukey=TukeyHSD(Anova_result)
tukey
plot(tukey)


