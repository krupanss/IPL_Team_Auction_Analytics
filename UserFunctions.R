# ***********************************************************Functions**********************************************************
fn_Hist_Plot_Con = function(xnames, histdata) {
  xvar = histdata[[xnames]]
  mean_data = mean(xvar)
  median_data = median(xvar)
  
  ggplot(data = histdata, aes(x=xvar)) + geom_histogram(color = 'black',  aes(fill=..count..)) +
    geom_vline(aes(xintercept=mean_data),
               color="red", linetype="dashed", size=1) +
    # geom_vline(aes(xintercept=median_data),
    #            color="blue", linetype="dashed", size=1) +
    # geom_density(col=3) +
    labs(x = xnames, title = paste("Histogram: ", xnames) )
}

fn_Hist_Plot_Cat = function(xnames, histdata) {
  xvar = histdata[[xnames]]
  ggplot(data = histdata, aes(x=xvar)) + geom_bar(color = 'black',  aes(fill=..count..)) +
    # geom_density(col=3) +
    labs(x = xnames, title = paste("Histogram: ", xnames), y = "Frequency" )
}

fn_Hist_Plot_ConGroup = function(xnames, histdata, hgroup) {
  xvar = histdata[[xnames]]
  mean_data = mean(xvar)
  median_data = median(xvar)
  hdata = histdata[[hgroup]]
  ggplot(data = histdata, aes(x=xvar, fill = as.factor(hdata))) + geom_histogram(color = 'black', position="dodge") +
    geom_vline(aes(xintercept=mean_data),
               color="red", linetype="dashed", size=1) +
    # geom_vline(aes(xintercept=median_data),
    #            color="blue", linetype="dashed", size=1) +
    # geom_density(col=3) +
    labs(x = xnames, title = paste("Histogram: ", xnames, "Vs ", hgroup), col = hgroup)
}

fn_Hist_Plot_CatGroup = function(xnames, histdata, hgroup) {
  xvar = histdata[[xnames]]
  hdata = histdata[[hgroup]]
  ggplot(data = histdata, aes(x=xvar, fill = as.factor(hdata))) + geom_bar(color = 'black', position="dodge") +
    # geom_density(col=3) +
    labs(x = xnames, title = paste("Histogram: ", xnames, "Vs ", hgroup), y = "Frequency" )
}

fn_BoxPlotCon = function(y_names, bdata, x_var) {
  ydata = bdata[[y_names]]
  ggplot(data = bdata, aes(x= x_var, y=ydata)) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=0.5, notch=FALSE, fill = 5) +
    xlab("") +
    labs(y = y_names, title = paste("Box Plot: ",y_names) )
}

fn_BoxPlotConGroup = function(y_names, bdata, x_var) {
  ydata = bdata[[y_names]]
  xdata = as.factor(bdata[[x_var]])
  # str(xdata)
  ggplot(data = bdata, aes(x= xdata, y=ydata)) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=0.5, notch=FALSE, fill = 5) +
    xlab("") +
    labs(y = y_names, title = paste("Box Plot: ",y_names, "Vs ", x_var) , x = x_var)
}

fn_BoxPlotCat = function(y_names, bdata) {
  ydata = bdata[[y_names]]
  ggplot(data = bdata, aes(x= factor(0), y=ydata)) + 
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=0.5, notch=FALSE, fill = 5) +
    xlab("") +
    labs( y = y_names , title = paste("Box Plot: ",y_names) )
}
