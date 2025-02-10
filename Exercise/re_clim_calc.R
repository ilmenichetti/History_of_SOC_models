

#installing and loading my own R package
library(devtools)
install_github("ilmenichetti/reclim", force=T)


library(re)
library(plyr)
library(xlsx)
library(RColorBrewer)




#read the data
Ekebo_field_data<-read.csv("Ekebo_field.csv")
Ekebo_weather_data<-read.csv("Ekebo_weather2.csv")

#average the data by treatment
Ekebo_field_data_ave<-ddply(Ekebo_field_data, c("year","treat"), summarise,
                           crop_id=unique(crop_id),
                           variance=unique(variance),
                           seeding=unique(seeding),
                           harvest=mean(harvest),
                           harvest2=mean(harvest2),
                           tillage=unique(tillage),
                           minimum_cover=mean(minimum_cover),
                           total_dm_kg_ha=mean(total_dm_kg_ha),
                           total_dm_kg_ha2=mean(total_dm_kg_ha2))


#Ekebo plot of the aboveground data
Ekebo_treatlist<-levels(as.factor(Ekebo_field_data_ave$treat))
Ekebo_year_ag<- Ekebo_field_data_ave[Ekebo_field_data_ave$treat==Ekebo_treatlist[1],]$year
Ekebo_total_dm_kg_ha<-Ekebo_field_data_ave[Ekebo_field_data_ave$treat==Ekebo_treatlist[1],]$total_dm_kg_ha
barplot(Ekebo_total_dm_kg_ha, names.arg=Ekebo_year_ag, las=2, xlab="Time",
        ylab=expression(paste("Total aboveground dry biomass (T  ",ha^-1,")")), col="darkseagreen4")


#calculate the re_clim (daily values, all treatments)
Ekebo_reclim_out<-reclim(weather=Ekebo_weather_data,
                         aboveground=Ekebo_field_data_ave,
                         latitude=55.98,
                         altitude=83,
                         sand=8,
                         clay=18,
                         ave_SOC=1.9,
                         depth=20,
                         sun.mode="Rsolar")

str(Ekebo_reclim_out$GAI)

png("GAI_Ekebo.png", width = 3000, height = 1500, res=300)
crop_id_used<-unique(Ekebo_reclim_out$crop_id)
palette_crop_id_used<-brewer.pal(length(crop_id_used), "Dark2")
plot(Ekebo_reclim_out$PET$date, Ekebo_reclim_out$GAI[3,], type="l", ylim=c(0, max(Ekebo_reclim_out$GAI[3,])*1.3), xlab="date", ylab="GAI")
for(i in 1:length(crop_id_used)){
  which_ones<-which(Ekebo_reclim_out$crop_id==unique(Ekebo_reclim_out$crop_id)[i])
  lines(Ekebo_reclim_out$PET$date[which_ones], Ekebo_reclim_out$GAI[3,][which_ones], col=palette_crop_id_used[i])
}
legend("topright", as.character(crop_id_used), col=palette_crop_id_used, bty="n", lty=1)
dev.off()

#calculate annual re-clim values
Ekebo_annual<-reclim_annual(Ekebo_reclim_out$results_daily)
where_re_crop<-grepl("re_crop.", as.character(colnames(Ekebo_annual)))
Ekebo_annual_re_crop<-(Ekebo_annual[where_re_crop])

write.xlsx(Ekebo_annual, "Ekebo_calculated_re.xlsx", sheetName="all_re_values")
write.xlsx(Ekebo_annual_re_crop, "Ekebo_calculated_re.xlsx", sheetName="re_clim", append=T)
#write.xlsx(Ekebo_reclim_out$results_daily, "Ekebo_calculated_re.xlsx", sheetName="re_clim_annual", append=T)
write.csv(Ekebo_reclim_out$results_daily, "Ekebo_calculated_daily_values.csv")
Ekebo_reclim_out$wilting_point
Ekebo_reclim_out$field_capacity








Ultuna_raw<-read.csv("Ultuna_raw_data.csv")
dim(Ultuna_raw)
colnames(Ultuna_raw)

Ultuna_raw_plyr<-ddply(Ultuna_raw, c("year","treat"), summarise,
                       amended_bool=unique(amended_bool),
                       amendment_C_kg_ha=mean(amendment_C_kg_ha))
Ultuna_raw_plyr$amendment_C_kg_ha[Ultuna_raw_plyr$amended_bool==T]=3000

write.csv(Ultuna_raw_plyr, file="Ultuna_amendments.csv")





