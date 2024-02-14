## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    fig.path = "figures/theory-",
    out.width = "100%"
)

## ----setup, results='hide', warning = FALSE, message = FALSE, include = FALSE----
library(ggplot2)
library(dplyr)

## -----------------------------------------------------------------------------
population <- c(1, 3, 0)

## -----------------------------------------------------------------------------
a <- 1
b <- -4
c <- 4
f <- function(x) {
    a * x^2 + b * x + c
}

## ----initial_poputation, out.width="70%", fig.cap=""--------------------------
# Define the domain over which we want to plot f(x)
x <- seq(from = 0, to = 4, length.out = 100)

# Define the domain over which we want to plot f(x) and create df data frame
df <- x |>
    data.frame(x = _) |>
    dplyr::mutate(y = f(x))

# Define the space over which we want to population to be
possible_xvalues <- seq(from = 0, to = 3, length.out = 4)

# Create space data frame
space <- possible_xvalues |>
    data.frame(x = _) |>
    dplyr::mutate(y = f(x))

# Calculate fitness inline
fitness <- population^2 - 4 * population + 4

# Selected the surviving parents
num_parents <- 2
selected_parents <- population |>
    order(fitness, decreasing = FALSE) |>
    head(num_parents)

# Plot f(x) using ggplot
ggplot2::ggplot(df, aes(x = x, y = y)) +
    geom_line(color = "black") + # Plot the function as a line
    geom_point(data = subset(space, x %in% c(1, 3)), color = "coral1", size = 3, shape = 8) + # Plot points at x=1 and x=3
    geom_point(data = subset(space, (x == 0)), color = "blue", size = 3, shape = 8) + # Plot a point at x=0
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + # Add horizontal line at y=0
    geom_vline(xintercept = 0, linetype = "dashed", color = "blue") + # Add vertical line at x=0
    theme_minimal()

## ----selection, out.width="70%", fig.cap=""-----------------------------------
# Plot f(x) using ggplot
ggplot(df, aes(x = x, y = y)) +
    geom_line(color = "black") + # Plot the function as a line
    geom_point(data = subset(space, x %in% c(1, 3)), color = "coral1", size = 6, shape = 8) + # Plot points at x=1 and x=3
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + # Add horizontal line at y=0
    geom_vline(xintercept = 0, linetype = "dashed", color = "blue") + # Add vertical line at x=0
    theme_minimal() # Use a minimal theme

## ----fitting, out.width="70%", fig.cap=""-------------------------------------
find.fitting <- function(a, b, c) {
    x_fitting <- -b / (2 * a)
    y_fitting <- f(x_fitting)
    c(x_fitting, y_fitting)
}
F <- find.fitting(a, b, c)

## ----fitting_plot, out.width="70%", fig.cap=""--------------------------------
# Plot f(x) using ggplot
ggplot(df, aes(x = x, y = y)) +
    geom_line(color = "black") + # Plot the function as a line
    geom_hline(yintercept = 0, linetype = "dashed") + # Add horizontal line at y=0
    geom_vline(xintercept = 0, linetype = "dashed") + # Add vertical line at x=0
    geom_point(x = F[1], y = F[2], shape = 18, size = 6, color = "red") + # Plot the vertex
    geom_text(x = F[1], y = F[2], label = "Fitting", vjust = -1, color = "red", size = 5) + # Add label next to the vertex
    theme_minimal() # Use a minimal theme

## ----function_intercepts------------------------------------------------------
# find the x-intercepts of f(x)
find.roots <- function(a, b, c) {
    discriminant <- b^2 - 4 * a * c
    if (discriminant > 0) {
        c((-b - sqrt(discriminant)) / (2 * a), (-b + sqrt(discriminant)) / (2 * a))
    } else if (discriminant == 0) {
        -b / (2 * a)
    } else {
        NaN
    }
}
solutions <- find.roots(a, b, c)

## ----plot_intercepts, out.width="70%", fig.cap=""-----------------------------
# Plot f(x) using ggplot
ggplot(df, aes(x = x, y = y)) +
    geom_line(color = "black") + # Plot the function as a line
    geom_hline(yintercept = 0, linetype = "dashed") + # Add horizontal line at y=0
    geom_vline(xintercept = 0, linetype = "dashed") + # Add vertical line at x=0
    geom_point(data = data.frame(x = solutions, y = rep(0, length(solutions))), shape = 18, size = 6, color = "red") + # Plot x-intercepts
    geom_text(data = data.frame(x = solutions, y = rep(0, length(solutions)), label = "Fitting(x-intercept)"), aes(label = label), vjust = -1, color = "red", size = 5) + # Add labels next to x-intercepts
    theme_minimal() # Use a minimal theme

