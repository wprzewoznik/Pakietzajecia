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

  out <- out %>%
    dplyr::mutate(Emisja = Nat * ((Alpha * Procent ^ 2 + Beta * Procent + Gamma + (Delta/Procent))/
                             (Epsilon * Procent ^ 2 + Zita * Procent + Hta) * (1-Reduction))
    ) %>%
    dplyr::select(Category, Fuel, Euro.Standard, Technology, Pollutant, Mode, Segment, Nat, Emisja)

  return(out)

}