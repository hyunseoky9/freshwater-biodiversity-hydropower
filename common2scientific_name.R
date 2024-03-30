#pulls out the browser with a google search with a common fish names
# so I can find the scientific names for each of the game species.
setwd('F:/sdm_modeling/spdata/game_sp')
gamesp = read.delim('game_species_list.csv', sep=',',comment.char = '#',header=T)
gamesp_list = unique(gamesp$common_name)
for(i in 1:length(gamesp_list)){
  url = sprintf('https://google.com/search?q=%s',gamesp_list[i])
  browseURL(url)
}

