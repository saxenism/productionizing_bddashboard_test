## Easy Test 1:
# Download 10,000 GBIF’s occurrence records of Mammals
# in the U.S (georeferenced records only), using the ‘rgbif’ R package.

# GBIF stands for Global Biodiversity Information Facility

# rgbif gives you access to data from GBIF via their REST API


# Install packages
install.packages('rgbif')
install.packages('bdvis')

# Load the installed packages
library('rgbif')
library('bdvis')

# taxonKey is a numeric key like 2435099
# occ_count(taxonKey = 2435099)
# Output: [1] 12921

# Mammalia class is identified by a key in the datasets from rgbif
mammalia_key <- name_suggest(q = 'Mammalia', rank = 'class')
mammalia_key <- mammalia_key[[1]][1]

# Before downloading, I'll check just how many records actually exist:
num_of_reqd_records <- occ_count(taxonKey = mammalia_key, 
                                 georeferenced = TRUE,
                                 country = "US")
print(num_of_reqd_records)
#Output: 2238824

#We need to download 10,000 from these 2,238,824 records.

# Setting the hasCoordinate parameter to true is so that our data is geo-referenced
mam_us_georef <- occ_search(taxonKey = mammalia_key,
                      scientificName = 'Mammalia',
                      country = 'US',
                      hasCoordinate = TRUE,
                      limit = 10000
                      )

# Coercing the received data(gbif class) into a data frame
mam_us_georef_df <- as.data.frame(mam_us_georef$data)

# Changing the above data frame to required formate
mam_us_georef_df_formatted <- format_bdvis(mam_us_georef_df, 
                                           source = "rgbif")

# Checking out the new data.frame
head(mam_us_georef_df_formatted, 2)

# Summary of the downloaded data
mam_data_summary <- bdsummary(indf = mam_us_georef_df_formatted)

# Checking out different graphs!!

# tempolar
tempolar(mam_us_georef_df_formatted, 
         color="green", 
         title="US Mammals daily", 
         plottype="r", 
         timescale="d") 
tempolar(mam_us_georef_df_formatted, 
         color="blue", 
         title="US Mammals weekly", 
         plottype="p", 
         timescale="w") 
tempolar(mam_us_georef_df_formatted, 
         color="red", 
         title="US Mammals monthly", 
         plottype="r", 
         timescale="m") 


# chronohorogram
chronohorogram(mam_us_georef_df_formatted)

# mapgrid
mapgrid(mam_us_georef_df_formatted,
        ptype="records",
        bbox=c(60,100,5,40),
        region = "US") 

mapgrid(mam_us_georef_df_formatted,
        ptype="presence",
        bbox=c(60,100,5,40),
        region=c("US"),
        gridscale=0.1) 

# bdcalendarheat
bdcalendarheat(mam_us_georef_df_formatted) 

# Saving the data as a CSV file
read.csv("./mammal_data.csv", encoding = 'latin1')



























############################################################################################################
## Failed Attempts
############################################################################################################

us_mammal_georef_data <- occ_data(taxonKey = mammalia_key,
                                  scientificName = 'Mammalia',
                                  country = 'US',
                                  hasCoordinate = TRUE,
                                  limit = 10000
)

us_mammal_georef_data9 <- occ_data(taxonKey = mammalia_key,
                                   scientificName = 'Mammalia',
                                   country = 'US',
                                   hasCoordinate = TRUE,
                                   limit = 9000
)

us_mammal_georef_data.frame <- as.data.frame(us_mammal_georef_data$data)

formatted_us_mammal_georef_data.frame <- format_bdvis(indf = us_mammal_georef_data.frame,
                                                      source = 'rgbif')
colnames(sample)
length(is.na(formatted_us_mammal_georef_data.frame$sex)) == nrow(formatted_us_mammal_georef_data.frame)
(formatted_us_mammal_georef_data.frame[ ,94])

remove_columns <- c()
flag = 0
sample <- formatted_us_mammal_georef_data.frame
for (column in 1:length(colnames(sample))) {
        if(!all(is.na(sample[, column]))) {
                print(column)
        }
}
populated_columns <- colSums(is.na(sample) != nrow(sample))
populated_columns[2][1]

length(z[!is.na(sample$key)])

remove_columns <- c()
test <- formatted_us_mammal_georef_data.frame
for(column in 1:length(colnames(test))) {
        if(sum(!is.na(test[, column])) != nrow(test)) {
                remove_columns <- append(column, remove_columns)
        }
}

test <- test[, -remove_columns]

bdsummary(formatted_us_mammal_georef_data.frame)
bdsummary(test)

# taxotree (requires gathering some API key... will do it later)
taxo_formatted_us_mammal_georef_data.frame <- gettaxo(mam_us_georef_df_formatted)
taxotree(taxo_formatted_us_mammal_georef_data.frame)
