#pull up downloaded gbif occurrence data and divide the data by species.
setwd('F:/sdm_modeling/spdata')
datasets = list()
filestr = c('./umn mussel/sp_occurrence_umn_mussel.csv','./usgs_fish/sp_occurrence_usgs_fish.csv','./thermal_tol/sp_occurrence_thermal_tol.csv','./nonindigenous/sp_occurrence_nonind.csv','./game_sp/sp_occurrence_gamesp.csv')
library(data.table)
done_sp = c()
for(i in 1:length(filestr))
{
  data = fread(filestr[i], sep = "\t", header = TRUE, na.strings = "\\N")
  for (s in data$species)
  {
    if(!(s %in% done_sp))
    {
      data$occurrenceStatus[data$occurrenceStatus=="PRESENT"] = 1
      data$occurrenceStatus[data$occurrenceStatus=="ABSENT"] = 0
      filename = sprintf('./per_sp/%s.csv',s)
      write.table(data[data$species==s], file = filename, sep = "\t",
                  row.names = FALSE, col.names=TRUE)
      done_sp = c(done_sp,s)
    }
  }
  sprintf("%s done",filestr[i])
}
save_sp_names = data.frame(species_name = done_sp)
write.csv(save_sp_names,'sp_list.csv')
