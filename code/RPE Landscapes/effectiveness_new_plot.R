#Taking as basis the effectiveness_plot from the repository (http://pedroj.github.io/effectiveness/)
#I have made some modifications to improve the display of my RPE landscapes

effectiveness_eq<- function(q1, q2, group=NA, label= NA, nlines=10,
                              myxlab= "QtComp", myylab= "QltComp")    {
  # q1 is the component to plot on X axis
  # q2 is the component to plot on Y axis
  # group is a group label
  # label is a taxa label
  require(devtools)
  require(ggplot2)
  require(ggrepel)
  #
  d<-as.data.frame(cbind(q1, q2, group, label))
  names(d)
  nlines <- nlines+1 # number of isolines wanted
  # slope of a straight line linking (left,bottom) to (right,above) 
  # corners of the graphic
  alfa <- max(d$q2)/max(d$q1)
  # sequence of (nlines) regular spaced x values for the isoclines
  xval <- seq(0, max(d$q1), 
              length.out=(nlines+1))[2:(nlines+1)] 
  isoc <- (xval*xval*alfa) # values of the isoclines
  vis1<-seq(0, max(d$q1), length.out= 1000)
  #
  pp<- as.data.frame(vis1) # Build dataset for within loop plot.
  for(i in 1:nlines)
  {
    pp<- cbind(pp, isoc[i]/vis1)
  }    
  # Main plot ------------------------------------------------------------
  # mytheme_bw.R
  # devtools::source_gist("https://gist.github.com/b843fbafa3af8f408972")
  # source("./R/mytheme_bw.R")
  #
  p1<- ggplot(d, aes(x= q1, y= q2)) + 
    geom_point(shape= group, size= 3) +
    geom_text_repel(aes(x= q1, y= q2), fontface="italic", #Include italics for the point names and modify ggrepel options
                    size= 2.6, label=label, 
                    box.padding = unit(0.6, "lines"), force=2,  
                    nudge_y= 0.7,
                    segment.size= 0.2, segment.alpha= 0.75) +
    # geom_text(size= 2, label= label, hjust= 0.5, vjust= 2.2) +
    theme(axis.text = element_text(size = rel(0.8)), 
                    axis.ticks = element_line(colour = "black"), 
                    legend.key = element_rect(colour = "grey80"), 
                    panel.background = element_rect(fill = "white", colour = NA), 
                    panel.border = element_rect(fill = NA, colour = "black"), 
                    panel.grid.major = element_line(colour = "grey90", size = 0.2), 
                    panel.grid.minor = element_line(colour = "grey98", size = 0.4), 
                    strip.background = element_rect(fill = "grey80", 
                    colour = "grey50", size = 0.2),
                    legend.text=element_text(face="italic"))        #Include italics for the legend
  # Repel labels
  # ggplot(d) +
  # geom_point(aes(q1, q2), color = 'red') +
  #    geom_text_repel(aes(wt, mpg, label = rownames(mtcars))) +
  #    theme_classic(base_size = 16)
  #
  # Adding isolines ------------------------------------------------------
  labelx<- rep(1.1*max(q1), nlines)             #Modified value from 0.8 to 1.1
  labely<- as.vector(t(pp[800,1:nlines+1]))
  
  for(i in 1:nlines+1){ 
    #labely<- isoc[i]/(0.8*max(sde$eff_per_vis)
    #    labely<- pp[,i][800]
    p1= p1 + geom_line(aes(x, y), 
                       data= data.frame(x= pp$vis1, y= pp[,i]), 
                       col="blue", size = 0.25, alpha= 0.6) + 
      ylim(0, max(q2))+ 
      xlim(0, 1.5*max(q1))+       #Included xlim - PROBLEM: if value of error bard goes beyond this lim it wont appear
      xlab(paste(myxlab)) + 
      ylab(paste(myylab))  # +
    #        geom_text(aes(), data= NULL, x= labelx, y= labely, 
    #            label = paste("QC = ", round(isoc[i], digits=1)),
    #            size = 4, colour = "red")
  }
  p1 + annotate("text", x= labelx, y= labely,
                label=paste("TE= ", round(isoc,1)), 
                size=4, colour="blue", hjust=0) 
} 



#Paco modified function
#' Function to plot effectiveness landscapes, with repel labels option.
#' 
#' @import ggplot2
#' @import ggrepel
#' @importFrom classInt classIntervals
#' 
#' @param q1 Numeric vector representing the "quantitative component", to plot on the X axis. 
#' @param q2 Numeric vector representing the "qualitative component", to plot on the Y axis.
#' @param q1.error Optional. Numeric vector to be used as error bars for \code{q1}.
#' @param q2.error Optional. Numeric vector to be used as error bars for \code{q2}.
#' @param pts.shape Optional. A grouping variable (< 7 groups) to set point shapes (e.g., family).
#' @param pts.color Optional. A grouping variable to set point colours (e.g., family).
#' @param label Optional. A character vector of the same length as \code{q1} and \code{q2} providing a label for the individual points (e.g., species acronym). Note that \code{label} may be NA for some points (useful to avoid overplotting of labels).
#' @param show.lines Logical. Show effectiveness isolines? (default is TRUE).
#' @param nlines Specify the number of isolines.
#' @param lines.breaks Either a numeric vector giving break points for the contour lines, or a \code{"style"} (e.g. \code{"quantile"}, \code{"equal"}, or \code{"pretty"}) to choose optimal breaks using \code{\link[classInt]{classIntervals}}. Note that using "pretty" will override \code{nlines}, as the number of lines will be determined algorithmically. See \code{\link[classInt]{classIntervals}} for more details.
#' @param lines.color Color of the isolines.
#' @param myxlab optional label for axis X.
#' @param myylab optional label for axis Y.
#'
#' @details The script plots effectiveness landscapes as described in Schupp, E. W., Jordano, P. and Gómez, J.M. 2010. Seed dispersal effectiveness revisited: a conceptual review. New Phytologist 188: 333-353.
#' 
#' @return A ggplot2 object, which can be later modified (see examples).
#' @export
#'
#' @examples
#' #------------------------------------------------------------------------
#' # Based on a dataset of Cecropia glaziovii frugivores.
#' # In this example we build the effectiveness landscape just for the 
#' # quantitative component, plotting its two subcomponents, visitation 
#' # rate and per-visit effectiveness.
#' #------------------------------------------------------------------------
#' data(cecropia)
#' effectiveness_plot(q1 = cecropia$totvis, q2 = cecropia$totbic, 
#'     myxlab= "No. visits/10h", 
#'     myylab="Effectiveness/vis (No. fruits handled)")
#' #------------------------------------------------------------------------
#' 
#' ## Avoiding label overplotting ##
#' ## e.g. showing only names of species with high effectiveness
#' labels = cecropia$code
#' labels[4:length(cecropia$code)] <- NA
#' effectiveness_plot(q1 = cecropia$totvis, q2 = cecropia$totbic, 
#'     label = labels, 
#'     myxlab= "No. visits/10h", 
#'     myylab="Effectiveness/vis (No. fruits handled)")
#'     
#'     
#' ################################################
#' 
#' ## Modify plot ##
#' myplot <- effectiveness_plot(q1 = cecropia$totvis, q2 = cecropia$totbic, 
#'     label = labels, nlines = 10, 
#'     myxlab= "No. visits/10h", 
#'     myylab="Effectiveness/vis (No. fruits handled)")
#' myplot + theme_minimal()
#' 
#'
effectiveness_paco <- function(q1, q2, 
                               q1.error = NULL, q2.error = NULL, 
                               pts.shape = NULL, pts.color = NULL, 
                               label = NA, 
                               show.lines = TRUE, nlines = 6,
                               lines.breaks = "quantile", lines.color = "grey50", 
                               myxlab= "QtComp", myylab= "QltComp")    {
  
  
  ## Some checks before starting the work...
  
  if (length(q1) != length(q2)) stop("q1 and q2 must have equal length")
  
  
  
  ### General elements ###
  
  d <- data.frame(x = q1, y = q2, label = label)  
  
  effplot <- 
    ggplot(data = d, aes(x, y)) +
    labs(x = myxlab, y = myylab) +
    theme(axis.text = element_text(size = rel(0.8)), 
          axis.ticks = element_line(colour = "black"), 
          legend.key = element_rect(colour = "grey80"), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "black"), 
          panel.grid.major = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.4), 
          strip.background = element_rect(fill = "grey80", 
                                          colour = "grey50", size = 0.2),
          legend.text=element_text(face="italic")) 
  
  
  
  ##### Plotting contour lines ####
  
  if (show.lines) {
    
    ### Fabricate contour lines ###
    
    ## Define lower and upper bounds ##
    x.lower <- ifelse(is.null(q1.error), 0, min(q1) - q1.error)     #Changed lim to 0
    x.upper <- ifelse(is.null(q1.error), max(q1), max(q1) + q1.error)
    y.lower <- ifelse(is.null(q2.error), 0, min(q2) - q2.error)     #Changed lim to 0
    y.upper <- ifelse(is.null(q2.error), max(q2), max(q2) + q2.error)
    
    ## Calculate values ##
    df <- expand.grid(x = seq(x.lower - 0.05*x.lower, x.upper + 0.05*x.upper, length.out = 500),
                      y = seq(y.lower - 0.05*y.lower, y.upper + 0.05*y.upper, length.out = 500))
    df$z <- df$x * df$y
    
    ## Define line breaks ##
    ## If not provided, using classIntervals to get nice breaks:
    ## e.g. style = "pretty" give nice rounded numbers for breaks (often ignoring nlines)
    ## style = "quantile" cover better the plot, but values are not rounded (uglier)
    ## there are other alternatives, see ?classIntervals
    
    if (is.numeric(lines.breaks)) {
      if (length(lines.breaks) != (nlines + 1)) stop("Length of lines.breaks does not match the specified number of lines (nlines + 1)")
      lbreaks <- lines.breaks
    }
    
    if (is.character(lines.breaks)) {
      lbreaks <- classInt::classIntervals(df$z, n = nlines + 1, 
                                          style = lines.breaks)$brks
    }
    
    
    
    
    #### Preparing curve labels ####
    brk <- lbreaks[-c(1, length(lbreaks))]
    xlabel <- rep(max(df$x) + 0.05*max(df$x), times = length(brk))
    ylabel <- brk/xlabel
    xy.labels <- data.frame(x = xlabel, y = ylabel, 
                            label = as.character(round(brk, 1)))   #Added one decimal
    xy.labels <- subset(xy.labels, y > y.lower)
    
    
    ### Add lines to plot ###
    effplot <- effplot +
      geom_contour(aes(x, y, z = z), data = df, colour = lines.color, breaks = lbreaks, size = 0.3) + 
      geom_text(aes(x, y, label = label), data = xy.labels)
    
    
  } 
  
  
  
  ### Error bars ###
  
  if (!is.null(q1.error)) {
    
    d <- data.frame(d, x.error = q1.error)
    effplot <- effplot + 
      geom_errorbarh(aes(xmin = x - x.error, xmax = x + x.error), data = d)
  }
  
  if (!is.null(q2.error)) {
    
    d <- data.frame(d, y.error = q2.error)
    effplot <- effplot + 
      geom_errorbar(aes(ymin = y - y.error, ymax = y + y.error), data = d)
  }
  
  
  
  
  
  ### Draw points ###
  
  if (is.null(pts.color) & is.null(pts.shape)) {
    effplot <- effplot +
      geom_point(size = 2)
  }
  
  if (is.null(pts.color) & !is.null(pts.shape)) {
    d <- data.frame(d, pts.shape)
    effplot <- effplot +
      geom_point(aes(shape = pts.shape), data = d, size = 2)
  }
  
  if (!is.null(pts.color) & is.null(pts.shape)) {
    d <- data.frame(d, pts.color)
    effplot <- effplot +
      geom_point(aes(colour = pts.color), data = d, size = 2)
  }
  
  if (!is.null(pts.color) & !is.null(pts.shape)) {
    d <- data.frame(d, pts.shape, pts.color)
    effplot <- effplot +
      geom_point(aes(color = pts.color, shape = pts.shape), 
                 data = d, size = 2)
  }
  
  
  
  ### Add point labels with ggrepel ###
  
  if (any(!is.na(label))) {
    
    effplot <- effplot +
      geom_text_repel(aes(x, y), data = d, size = 3, label = label, 
                      nudge_y = 0.5, 
                      segment.size = 0.2, segment.alpha = 0.75) 
  }
  
  
  ## Print and return ggplot object
  effplot
  
}
