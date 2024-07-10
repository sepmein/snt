#' @export
#' @title Harmonic Analysis
#' @param rainfall_data Rainfall data, should be monthly rainfall data
#' @importFrom stats fft
#' @importFrom stats Mod
#' @description Calculate the harmonic analysis of rainfall data
#' if the ratio of the second to the first harmonic is greater than 1.0
#' the data is considered to be bimodal, otherwise unimodal
#' @examples harmonic_analysis(rainfall_data)
harmonic_analysis <- function(rainfall_data) {
    # Number of data points
    n <- length(rainfall_data)

    # Apply FFT
    fft_result <- fft(rainfall_data)

    # Calculate amplitudes
    amplitude_spectrum <- Mod(fft_result)

    # Calculate the corresponding frequencies
    freqs <- (0:(n - 1)) / n

    # Identify dominant harmonics
    first_harmonic <- amplitude_spectrum[2]
    second_harmonic <- amplitude_spectrum[3]

    # Calculate the ratio of second to first harmonic
    harmonic_ratio <- second_harmonic / first_harmonic

    # Determine the regime based on the ratio
    regime <- ifelse(harmonic_ratio > 1.0, "Bimodal", "Unimodal")
}