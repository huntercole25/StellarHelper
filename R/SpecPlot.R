#' Field Plotting Function
#'
#' This function creates a dark-themed plot for quickly checking exported spectral data while maintaining the integrity of your scoptopic vision.
#' @param FolderOfInterest A character string providing the path to the directory housing the file of interest.
#' @param Filename A character string providing the file name for the file of interest WITHOUT EXTENSION.
#' @param FileExtension A character string providing the file extension of the file of interest WITHOUT THE LEADING DOT. This defaults to "ssm" which corresponds with scope mode.
#' @param Title A character string providing the desired plot title. Defaults to the file name.
#' @param Ylab A character string providing the desired y-axis label. Defaults to "Count" which is appropriate for data collected in scope mode.
#' @keywords plotting
#' @export
#' @examples
#' #Plotting a .ssm file
#' SpecPlotter("~/Documents", "Test_File")
#'
#' #Plotting a .irr file
#' SpecPlotter("~/Documents", "Test_File2", "irr")

#' @export
SpecPlotter <- function(FolderOfInterest, Filename, FileExtension = "ssm", Title = Filename, Ylab = "Count"){
  require(ggplot2)
  require(pavo)

  SpecData <- pavo::getspec(where = FolderOfInterest, ext = FileExtension, lim = c(250, 1100))

  SpecPlot <- ggplot2::ggplot(data = SpecData, aes(x = wl, y = SpecData[[Filename]])) +
    ggplot2::geom_line(color = "red") +
    ggplot2::scale_x_continuous(breaks = seq(250, 1100, 50)) +
    ggplot2::labs(x = "Wavelength (nm)", y = Ylab, title = Title) +
    ggplot2::geom_vline(xintercept = pavo::peakshape(SpecData, plot = F)$H1, color = "grey80", linetype = "dashed") +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "black"), panel.background = ggplot2::element_rect(fill = "black"),
          panel.grid.major = ggplot2::element_line(color = "grey50"), panel.grid.minor = ggplot2::element_line(color = "black"),
          panel.grid.major.y = ggplot2::element_line(color = "black"), axis.text = ggplot2::element_text(color = "grey70", size = 14),
          legend.background = ggplot2::element_rect(fill = "black"), legend.text = ggplot2::element_text(color = "white"),
          legend.title = ggplot2::element_text(color = "white"), legend.key = ggplot2::element_rect(fill = "black", color = "black"),
          plot.title = ggplot2::element_text(color = "grey70", hjust = 0.5, margin = margin(0,0,10,0), size = 18, face= "bold"),
          axis.title = ggplot2::element_text(color = "grey70", size = 16, face = "bold"),
          axis.title.x = ggplot2::element_text(margin = margin(10,0,0,0)), axis.title.y = ggplot2::element_text(margin = margin(0,10,0,0)))

  plot(SpecPlot)
}

SpecPlotter(FolderOfInterest = "~/Downloads/", Filename = "CFL_test")

