install.packages("prism")

remove.packages("raster")
install.packages("raster")

require(prism)

detach("package:prism", unload=TRUE)

require(raster)

#### Before downloading any data we set the directory to make sure the files are downloaded to this path
prism_set_dl_dir("/Users/Dinesh/Dinesh/Research/CattleModel/Data/New/PRISM/TMIN")

get_prism_annual(type = 'tmin', year = 1990:2020, keepZip = FALSE)

#### Grab the prism data and compile all the files
climateTMIN_data <- prism_archive_ls() %>% pd_stack()

##convert raster to point data frame
TMIN_df <- data.frame(rasterToPoints(climateTMIN_data))
m.df <- melt(TMIN_df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat")

min(TMIN_df$PRISM_tmin_stable_4kmM3_2020_bil)








#### Grab the prism data and compile all the files
climateTMAX_data <- prism_archive_ls() %>% pd_stack()

##convert raster to point data frame
TMAX_df <- data.frame(rasterToPoints(climateTMAX_data))
m.df <- melt(TMAX_df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") 

tmaxAVGS <- m.df %>% group_by(variable) %>% mutate(meanTEMP = mean(value)) %>% as.data.frame()



##### Here we are getting prism data. For our analysis we use the yearly data
get_prism_annual(type = 'tmax', year = 1990:2020, keepZip = FALSE)