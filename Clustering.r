### KMeans Clustering Toolbox
##Author: Fabio Veronesi
tool_exec <- function(in_params, out_params)
{
  if (!requireNamespace("sp", quietly = TRUE))
    install.packages("sp")
  require(sp)
  
  print("K-Means Clustering of Shapefiles")
  print("Author: Fabio Veronesi")

  source_dataset = in_params[[1]]
  nclust = in_params[[2]]
  variable = in_params[[3]]

  out_shape = out_params[[1]]
   
  ### Read Data
  arc.progress_label("Loading Dataset")
  d <- arc.open(source_dataset)
  

  ### Create a Data.Frame with the variables to cluster
  data <- arc.select(d, variable)

  data_clust <- data.frame(data[,variable[1]])
  
  if(length(variable)>1){
	for(i in 2:length(variable)){
		data_clust <- cbind(data_clust,data[,variable[i]])
	}
  }
  
  names(data_clust) <- variable
  
  for(i in 1:length(variable)){
	dev.new()
	plot(hist(data_clust[,i]),main=paste0("Histogram of ",variable[i]),xlab=variable[i])
  }
  
  clusters <- kmeans(data_clust, nclust)
    
  result <- data.frame(cluster=clusters$cluster)
  
  arc.write(out_shape, result, coords = arc.shape(data))

  print("Done")
  return(out_params)
}
