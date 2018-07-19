

#***************************************************************
#
#  Plots Function for Transplant Data
#
#***************************************************************

#***************************************************************
#
#  donortype.plot
#
#***************************************************************


# parameters

# d_matrix: a matrix transplants from living and deceased donors (2 columns) and living and deceased donors (2 columns)

# title for the graph

# optional years, labels and colors for plotting.
			
			

donortype.plot <- function(d_matrix, title, Year = seq(1988, 2016, 1), labels = c("DD Trans.", "LD Trans.", "DD", "LD"), col = c("red", "blue", "green", "purple"))
{
	lb <- min(apply(d_matrix, 2, min))
	ub <- max(apply(d_matrix, 2, max))
	par(xpd = T)
	par(plt = c(0.1171429, 0.8, 0.1457143, 0.8828571))
	if(ncol(d_matrix) < 3)
	{
		on.exit(cat("Matrix is too small"))
		}
	if(ncol(d_matrix) < 4)
	{
		rc <- as.numeric(readline("Which column is missing?  "))
		if(rc <1 | rc > 3)
		{
			on.exit(cat("Column number not valid"))
			}
		labels <- labels[-rc]
		col <- col[-rc]
		}
	plot(d_matrix[,1]~Year, type = "l", col = col[1], ylim = c(lb, ub), main = title, ylab = "Number", xlab = "Year")
	cm <- ncol(d_matrix)
	for(i in 2:cm)
	{
		lines(d_matrix[,i]~Year,  col = col[i])
		}
	
	legend(2016.5, ub-lb, legend = labels, col = col, lwd = 1, cex = .8)
	par(plt = c(0.1171429, 0.9400000, 0.1457143, 0.8828571))
	par(xpd = F)
	}


#***************************************************************
#
#  region.plot
#
#***************************************************************


# parameters

# matrix: a matrix transplants and donors from a region and transplants from four centers in that region.

# title for the graph

# optional years, labels and colors for plotting.
			


region.plot <- function(M, title, Year = seq(1988, 2016, 1), labels = c("R11 Trans.", "R11 Donor.", "UVA", "UNC", "MCV", "Duke"), col =c("red", "blue", "orange", "lightblue3", "purple", "blue3"))
{
	lb <- min(apply(M, 2, min))
	ub <- max(apply(M, 2, max))
	par(xpd = T)
	par(plt = c(0.1171429, 0.8, 0.1457143, 0.8828571))
	if(ncol(M) < 6)
	{
		on.exit(cat("Matrix is too small"))
		}
		plot(M[,1]~Year, type = "l", col = col[1], ylim = c(lb, ub), main = title, ylab = "Number", xlab = "Year")
	cm <- ncol(M)
	for(i in 2:cm)
	{
		lines(M[,i]~Year,  col = col[i])
		}
	
	legend(2016.5, ub-lb, legend = labels, col = col, lwd = 1, cex = .8)
	par(plt = c(0.1171429, 0.9400000, 0.1457143, 0.8828571))
	par(xpd = F)
	}

#***************************************************************
#
#  center.plot
#
#***************************************************************


# parameters

# matrix: a matrix transplants done by four centers

# title for the graph

# optional years, labels and colors for plotting.

center.plot <- function(M, title, Year = seq(1988, 2016, 1), labels = c( "UVA", "UNC", "MCV", "Duke"), col =c( "orange", "lightblue3", "purple", "blue3"))
{
	lb <- min(apply(M, 2, min))
	ub <- max(apply(M, 2, max))
	par(xpd = T)
	par(plt = c(0.1171429, 0.8, 0.1457143, 0.8828571))
	if(ncol(M) < 4)
	{
		on.exit(cat("Matrix is too small"))
		}
		plot(M[,1]~Year, type = "l", col = col[1], ylim = c(lb, ub), main = title, ylab = "Number", xlab = "Year")
	cm <- ncol(M)
	for(i in 2:cm)
	{
		lines(M[,i]~Year,  col = col[i])
		}
	
	legend(max(Year)+1.2, ub-lb, legend = labels, col = col, lwd = 1, cex = .8)
	par(plt = c(0.1171429, 0.9400000, 0.1457143, 0.8828571))
	par(xpd = F)
	}


