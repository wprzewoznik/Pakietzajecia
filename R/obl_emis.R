#' Funkcja obliczajaca wskaznik emisji la danych na podstawie podanych,
#' zaimportowanych parametrow
#'
#' @param dane char
#' @param kategoria char
#' @param paliwo char
#' @param euro char
#' @param technologia char
#' @param mode char
#' @param substancja char
#'
#' @return double
#'
#'
#' @export
#'
#'
#' @examples
#' #obl_emis() -> tmp
#' #tmp %>% glimpse()
#'
obl_emis <- function(dane = input,
                     kategoria = "Passenger Cars",
                     paliwo = "Petrol",
                     #segment = "Mini",
                     euro = "Euro 5",
                     technologia = "GDI",
                     mode = "",
                     substancja = c("EC", "CO")) {

  # Zabezpieczenia argumentów funkcji i danych wejscioweych.

    dplyr::filter(wskazniki, Category %in% kategoria) -> out
    dplyr::filter(out, Fuel %in% paliwo) -> out
    dplyr::filter(out, Euro.Standard %in% euro) -> out
    dplyr::filter(out, Technology %in% technologia) -> out
    dplyr::filter(out, Pollutant %in% substancja) -> out
    dplyr::filter(out, Mode %in% mode) -> out

  out <- dplyr::inner_join(x = out, y = input, by = "Segment")

    dplyr::mutate(out, Emisja = Nat * ((Alpha * Procent ^ 2 + Beta * Procent + Gamma + (Delta/Procent))/
                             (Epsilon * Procent ^ 2 + Zita * Procent + Hta) * (1-Reduction)))

    dplyr::select(out, Category, Fuel, Euro.Standard, Technology, Pollutant, Mode, Segment, Nat, Emisja)

  return(out)

}
