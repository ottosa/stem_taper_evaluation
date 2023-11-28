
# Function to read TLS data

# f = list of files
# dir_path = folder where the files are
# d_frame = data frame of field data  

read_trees <- function(f, dir_path, d_frame){
  
  # Empty data.frame() to store the data
  data <- data.frame()
  
  # loop reads files one by one from f
  for (i in 1:length(f)){
    
    # read in .txt file from dir_path
    f_name <- paste0(dir_path,f[i]) # f_name = directory path
    d <- read.delim(f_name, sep = " ", head = F) # reading the .txt file in
    names(d) <- c("X","Y","Z","D") # rename columns
    
    # Setting height mesurements starting from 0
    d$Z = d$Z - min(d$Z) # measurement height = 0
    
    id <- substr(f[i],1,nchar(f[i])-4) # specify tree id
    stand <- unique(d_frame$Stand[d_frame$ID == id]) # specify tree stand
    
    # Tree height from field measurements
    tree <- d_frame[d_frame$ID == id,] 
    h <- tree$H # Tree height
    d <- rbind(d,c(0,0,h,0)) #adding tree height to measurement height 0
    
    d$id <- id # adding tree id
    d$stand <- stand # adding tree stand 
    data <- rbind(data,d) # adding tree data data to data.frame
  }
  
  data <- data[, c(5,6,1,2,3,4)] # re-organize colums  
  
  # re-organize by id and height  
  data <- data %>% arrange(id,Z)
  
  # return data.frame
  return(data)
}
