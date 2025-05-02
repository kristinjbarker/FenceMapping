## *Functions are specific to summer 2024 field season but are easily modified

# Clean and standardize names of data collectors
clean_collector <- function(name) {
  # Common typos/variations, IDed from rawCollectors
  if (str_detect(name, "lliso|bald|basin")) {
    return("Allison Stoff") 
  } else if (str_detect(name, "eth full")) {
    return("Beth Fuller")
  } else if (str_detect(name, "lemi|tristen|tristn")) {
    return("Tristen Fleming")
  } else if (str_detect(name, "hayden")) {
    return("Hayden Campbell")
  } else if (str_detect(name, "ean jeff")) {
    return("Dean Jeffers")
  } else if (str_detect(name, "bha")) {
    return("Andrew Hahne")
  } else if (str_detect(name, "^er$")) {
    return("Emily Reed")
  } else if (str_detect(name, "brut")) {
    return("Jaden Brutsman")
  } else if (str_detect(name, "kimi")) {
    return("Kimi Zamuda")
  }
  # Make names title case
  return(str_to_title(name))
}


# Match names to organizations
clean_org <- function(name) {
  if (str_detect(name, "Allison Stoff|Tristen Fleming|Hayden Campbell|Jaden Brutsman|Kristin Barker")) {
    return("BYLL")
  } else if (str_detect(name, "Kimi")) {
    return("TNC") 
  } else if (str_detect(name, "Emily Reed")) {
    return("AFI")
  } else if (str_detect(name, "Andrew Hahne|Madison Clarke|Dean Jeffers")) {
    return("BHA")
  } 
  return("UnkOther")
}
