#' Field Data Visualization Function
#'
#' This function creates a dark-themed plot for quickly visualizing spectral data while maintaining the integrity of your scoptopic vision.
#' @param FilePath A character string providing the full file path to the spectral file of interest including file extension.
#' @param Lims A numeric vector indicating the wavelength limits to be read in via `pavo::getspec()`. This vector cannot go outside of the wavelengths in the file of interest. Defaults to c(250, 1100).
#' @param ObjectSave A Boolean value indicating whether the spectral data should be saved as an object in the global environment. Defaults to T.
#' @param ObjectName A character string indicating the name of the rspec object that is saved to the global environment if `ObjectSave = T`.
#' @param BreakInt An integer inidcating the x-axis label interval in nanometers. Defaults to 50.
#' @param Vgrid A Boolean value indicating whether the major grid lines should be visible. Defaults to F.
#' @param Title A character string providing the desired plot title. Defaults to the file name.
#' @param Ylab A character string providing the desired y-axis label. Defaults to "Count" which is appropriate for data collected in scope mode.
#' @param SavePlot A Boolean value indicating whether the plot this function generates should be saved as a .png file in the same directory and with the same name as the spectral file. Defaults to F.
#' @keywords plotting
#' @export
#' @examples
#' ##Not run:
#' #Plotting a .SSM file
#' SpecPlotter("~/Documents/example_file.ssm")
#' 
#' #Plotting a .IRR file from 500nm-700nm
#' SpecPlotter("~/Documents/example_file.IRR", Lims = c(500,700))

#' @export
SpecPlotter <- function(FilePath, Lims = c(250, 1100), ObjectSave = T, ObjectName = paste0(Filename, "_spec"), BreakInt = 50,
                        Vgrid = F, Title = basename(FilePath), Ylab = "Count", SavePlot = F){
  require(ggplot2)
  require(pavo)
  
  FOI <- dirname(FilePath)
  Base <- basename(FilePath)
  FileExtension <- regmatches(Base, regexpr("[[:alnum:]]+$", Base))
  Filename <- sub("(.+)\\.[[:alnum:]]+$", "\\1", Base)

  SpecDataRaw <- pavo::getspec(where = FOI, ext = FileExtension, lim = Lims)
  SpecData <- SpecDataRaw[, c("wl", Filename)]
  
  if(ObjectSave == T){
    assign(x = ObjectName, value = SpecData, pos = .GlobalEnv)
  }

  SpecPlot <- ggplot2::ggplot(data = SpecData, aes(x = wl, y = .data[[Filename]])) +
    ggplot2::geom_line(color = "red", size = 0.8) +
    ggplot2::scale_x_continuous(breaks = seq(min(SpecData$wl), max(SpecData$wl), BreakInt)) +
    ggplot2::scale_y_continuous(limits = c(0, max(SpecData[[Filename]])))+
    ggplot2::labs(x = "Wavelength (nm)", y = Ylab, title = Title) +
    ggplot2::geom_vline(xintercept = pavo::peakshape(SpecData, plot = F)$H1, color = "grey80", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, color = "grey50") +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "black"), panel.background = ggplot2::element_rect(fill = "black"),
          panel.grid.major = ggplot2::element_line(color = "black"), panel.grid.minor = ggplot2::element_line(color = "black"),
          axis.text = ggplot2::element_text(color = "grey70", size = 14), axis.ticks = ggplot2::element_line(color = "grey50"),
          legend.background = ggplot2::element_rect(fill = "black"), legend.text = ggplot2::element_text(color = "white"),
          legend.title = ggplot2::element_text(color = "white"), legend.key = ggplot2::element_rect(fill = "black", color = "black"),
          plot.title = ggplot2::element_text(color = "grey70", hjust = 0.5, margin = margin(0,0,10,0), size = 18, face= "bold"),
          axis.title = ggplot2::element_text(color = "grey70", size = 16, face = "bold"), axis.line.y = ggplot2::element_line(color = "grey50"),
          axis.title.x = ggplot2::element_text(margin = margin(10,0,0,0)), axis.title.y = ggplot2::element_text(margin = margin(0,10,0,0)))

  if(Vgrid == T){
    SpecPlot <- SpecPlot + ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = "grey50"))
  }
  
  if(SavePlot == T){
    ggplot2::ggsave(filename = paste0(Filename, "_Spectrum.png"), path = FOI, device = "png", width = 12, height = 7, units = "in")
  }
  
  plot(SpecPlot)
}
