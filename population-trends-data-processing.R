library(tidyverse)
library(openxlsx)
library(psrcelmer)
library(echarts4r)
library(psrcplot)
library(ggplot2)
library(treemapify)

source("functions.R")

install_psrc_fonts()

# Data Inputs -------------------------------------------------------------

ofm_1990 <- "X:/DSA/population-trends/data/ofm_april1_intercensal_estimates_1990-2000.xlsx"
ofm_2000 <- "X:/DSA/population-trends/data/ofm_april1_intercensal_estimates_2000-2010.xlsx"
ofm_2010 <- "X:/DSA/population-trends/data/ofm_april1_intercensal_estimates_2010_2020.xlsx"
ofm_2020 <- "X:/DSA/population-trends/data/ofm_april1_draft.xlsx"

base_year <- 2018
pre_covid <- 2019

# Jurisdictions -----------------------------------------------------------
jurisdictions <- get_table(schema='Political', tbl_name='jurisdiction_dims')

jurisdictions <- jurisdictions |>
  mutate(juris_name = str_replace_all(juris_name, "Seatac", "SeaTac")) |>
  mutate(juris_name = str_replace_all(juris_name, "Beau Arts Village", "Beaux Arts Village")) |>
  select(geography="juris_name", "regional_geography") |>
  distinct() |>
  mutate(regional_geography = str_replace_all(regional_geography, "HCT", "HCT Community")) |>
  mutate(regional_geography = str_replace_all(regional_geography, "Metro", "Metropolitan Cities")) |>
  mutate(regional_geography = str_replace_all(regional_geography, "Core", "Core Cities")) |>
  mutate(regional_geography = str_replace_all(regional_geography, "CitiesTowns", "Cities & Towns")) |>
  mutate(geography = str_replace_all(geography, "Uninc. King", "King County")) |>
  mutate(geography = str_replace_all(geography, "Uninc. Kitsap", "Kitsap County")) |>
  mutate(geography = str_replace_all(geography, "Uninc. Pierce", "Pierce County")) |>
  mutate(geography = str_replace_all(geography, "Uninc. Snohomish", "Snohomish County"))

# Process Population Data ----------------------------------------------
ofm_pop_90 <- as_tibble(read.xlsx(ofm_1990, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE, sheet = "Total Population ")) |>
  filter(County.Name %in% c("King","Kitsap","Pierce","Snohomish")) |>
  pivot_longer(cols=contains("Population"), names_to="Year", values_to="Estimate") |>
  select(-(contains("Place.Code")), -"Jurisdiction") |>
  mutate(Year = str_replace(Year, ".Total.Population.*", ""), City.Name = str_replace(City.Name, " \\(part\\)", "")) |>
  mutate(across(c('Filter','Year','Estimate'), as.numeric)) |>
  rename(County="County.Name", Jurisdiction="City.Name") |>
  filter(Year != 2000) |>
  mutate(Jurisdiction = case_when(
    Filter == 1 ~ paste0(County," County"),
    Filter == 2 ~ paste0(Jurisdiction," ", County, " County"),
    Filter == 3 ~ paste0(Jurisdiction," ", County," County"),
    Filter == 4 ~ Jurisdiction)) |>
  mutate(Jurisdiction = str_replace(Jurisdiction, "Beaux Arts", "Beaux Arts Village")) |>
  mutate(Jurisdiction = str_replace(Jurisdiction, "Du Pont", "DuPont")) |>
  mutate(Jurisdiction = str_trim(Jurisdiction, side = c("both")))

ofm_pop_00 <- as_tibble(read.xlsx(ofm_2000, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE, sheet = "Total Population")) |>
  filter(County.Name %in% c("King","Kitsap","Pierce","Snohomish")) |>
  pivot_longer(cols=contains("Population"), names_to="Year", values_to="Estimate") |>
  select(-(contains("Place.Code")), -"Jurisdiction") |>
  mutate(Year = str_replace(Year, ".Total.Population.*", ""), Year = str_replace(Year, ".Intercensal.*", ""), City.Name = str_replace(City.Name, " \\(part\\)", "")) |>
  mutate(across(c('Filter','Year','Estimate'), as.numeric)) |>
  rename(County="County.Name", Jurisdiction="City.Name") |>
  filter(Year != 2010) |>
  select(-"Line") |>
  mutate(Jurisdiction = case_when(
    Filter == 1 ~ Jurisdiction,
    Filter == 2 ~ paste0(Jurisdiction," ", County, " County"),
    Filter == 3 ~ paste0(Jurisdiction," ", County, " County"),
    Filter == 4 ~ Jurisdiction)) |>
  mutate(Jurisdiction = str_trim(Jurisdiction, side = c("both")))

ofm_pop_10 <- as_tibble(read.xlsx(ofm_2010, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE, sheet = "Total Population")) |>
  filter(County.Name %in% c("King","Kitsap","Pierce","Snohomish")) |>
  pivot_longer(cols=contains("Population"), names_to="Year", values_to="Estimate") |>
  select(-(contains("FIPS.Code")), -"Jurisdiction") |>
  mutate(Year = str_replace(Year, ".Total.Population.*", ""), Year = str_replace(Year, ".Intercensal.*", ""), City.Name = str_replace(City.Name, " \\(part\\)", "")) |>
  mutate(across(c('Filter','Year','Estimate'), as.numeric)) |>
  rename(County="County.Name", Jurisdiction="City.Name") |>
  filter(Year != 2020) |>
  select(-"Line") |>
  mutate(Jurisdiction = case_when(
    Filter == 1 ~ Jurisdiction,
    Filter == 2 ~ paste0(Jurisdiction," ", County, " County"),
    Filter == 3 ~ paste0(Jurisdiction," ", County, " County"),
    Filter == 4 ~ Jurisdiction)) |>
  mutate(Jurisdiction = str_trim(Jurisdiction, side = c("both")))

ofm_pop_20 <- as_tibble(read.xlsx(ofm_2020, detectDates = FALSE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE, sheet = "ofm_april1_draft")) |>
  filter(County.Name %in% c("King","Kitsap","Pierce","Snohomish")) |>
  pivot_longer(cols=contains("Population"), names_to="Year", values_to="Estimate") |>
  mutate(Year = str_replace(Year, "Population.", ""), City.Name = str_replace(City.Name, " \\(part\\)", "")) |>
  mutate(across(c('Filter','Year','Estimate'), as.numeric)) |>
  rename(County="County.Name", Jurisdiction="City.Name") |>
  mutate(Jurisdiction = case_when(
    Filter == 1 ~ Jurisdiction,
    Filter == 2 ~ paste0(Jurisdiction," ", County, " County"),
    Filter == 3 ~ paste0(Jurisdiction," ", County, " County"),
    Filter == 4 ~ Jurisdiction)) |>
  mutate(Jurisdiction = str_trim(Jurisdiction, side = c("both")))

ofm_pop <- bind_rows(list(ofm_pop_90, ofm_pop_00, ofm_pop_10, ofm_pop_20)) |>
  group_by(Filter, Jurisdiction, Year) |>
  summarize(Estimate = sum(Estimate)) |>
  as_tibble() |>
  select(filter = "Filter", year = "Year", geography = "Jurisdiction", total_population = "Estimate")

region_pop <- ofm_pop |>
  filter(filter <= 3) |>
  select("filter", "year", "total_population") |>
  group_by(filter, year) |>
  summarize_all(sum) |>
  mutate(geography = "Region") |>
  mutate(geography = ifelse(filter == 2, "Unincorporated Region", geography)) |>
  mutate(geography = ifelse(filter == 3, "Incorporated Region", geography))

ofm_pop <- bind_rows(ofm_pop, region_pop)

ofm_pop <- left_join(ofm_pop, jurisdictions, by=c("geography")) |>
  mutate(regional_geography = case_when(
    filter==1 ~ "County",
    filter==2 ~ "Unincorporated",
    filter==3 ~ "Incorporated",
    filter==4 ~ regional_geography))

rm(ofm_pop_90, ofm_pop_00, ofm_pop_10, ofm_pop_20, region_pop, jurisdictions)

# Annual Change -----------------------------------------------------------
ofm_pop <- ofm_pop |>
  group_by(geography) |>
  mutate(previous = (lag(total_population))) |>
  drop_na() |>
  mutate(total_change = total_population - previous) |>
  mutate(percent_change = total_change / previous) |>
  select("filter", "year", "geography", "regional_geography", "total_population", "total_change", "percent_change") |>
  as_tibble()

# Share Calculations ------------------------------------------------------
region <- ofm_pop |> filter(geography == "Region") |> select(year, region_population = "total_population", region_change = "total_change")

ofm_pop <- left_join(ofm_pop, region, by=c("year")) |>
  mutate(share_region_total = total_population / region_population) |>
  mutate(share_region_change = total_change / region_change) |>
  select(-"region_population", -"region_change")

rm(region)

# Charts ------------------------------------------------

region_pop_change_chart <- create_bar_chart(df = ofm_pop |> 
                                              filter(geography  == "Region" & year >= base_year) |>
                                              mutate(year = as.character(year), metric = "Annual Population Change"),
                                            x = "year", y = "total_change", fill = "metric", color = c("#91268F"), legend = FALSE, left_align='15%', bottom_padding=50)

geography_pop_change_chart <- create_static_treemap_chart(t = ofm_pop |> 
                                                            filter(filter %in% c(2,4) & year >= base_year) |>
                                                            filter(!(str_detect(geography, "County"))) |>
                                                            group_by(regional_geography) |>
                                                            summarise(estimate = sum(total_change)) |>
                                                            as_tibble(),
                                                          area = "estimate", fill = "regional_geography",
                                                          est = "number", dec = -2)

county_pop_change_chart <- echart_pie_chart(t = ofm_pop |> 
                                              filter(filter ==1 & geography !="Region" & year >= base_year) |>
                                              group_by(geography) |>
                                              summarise(estimate = sum(total_change)) |>
                                              as_tibble(),
                                            val = "estimate",
                                            lab = "geography",
                                            color = c("#91268F", "#F05A28", "#8CC63E", "#00A7A0"),
                                            legend=FALSE)


hct_pop_change_chart <- echart_pie_chart(t = ofm_pop |> 
                                        filter(regional_geography == "HCT Community" & year >= base_year) |>
                                        group_by(geography) |>
                                        summarise(estimate = sum(total_change)/sum(total_population)) |>
                                        as_tibble(),
                                        val = "estimate", lab = "geography",
                                        color = c("#91268F", "#F05A28", "#8CC63E", "#00A7A0"),
                                                          legend = FALSE)

hct_pop_change_chart_bar <- create_bar_chart(df = ofm_pop |> 
                                               filter(regional_geography == "HCT Community" & year >= base_year) |>
                                               group_by(geography) |>
                                               summarise(estimate = sum(total_change)/sum(total_population)) |>
                                               as_tibble() |> 
                                               arrange(estimate) |>
                                               mutate(metric = "HCT Pop Growth"),
                                            x = "geography", y = "estimate", fill = "metric", 
                                            bar_column = "bar", dec = 1,
                                            esttype = "percent", color = c("#91268F"), legend = FALSE, left_align='25%', bottom_padding=50)

core_pop_change_chart_bar <- create_bar_chart(df = ofm_pop |> 
                                               filter(regional_geography == "Core Cities" & year >= base_year) |>
                                               group_by(geography) |>
                                               summarise(estimate = sum(total_change)/sum(total_population)) |>
                                               as_tibble() |> 
                                               arrange(estimate) |>
                                               mutate(metric = "Core Cities Pop Growth"),
                                             x = "geography", y = "estimate", fill = "metric", 
                                             bar_column = "bar", dec = 1,
                                             esttype = "percent", color = c("#91268F"), legend = FALSE, left_align='25%', bottom_padding=50)


metro_pop_change_chart_bar <- create_bar_chart(df = ofm_pop |> 
                                                filter(regional_geography == "Metropolitan Cities" & year >= base_year) |>
                                                group_by(geography) |>
                                                summarise(estimate = sum(total_change)/sum(total_population)) |>
                                                as_tibble() |> 
                                                arrange(estimate) |>
                                                mutate(metric = "Metro Cities Pop Growth"),
                                              x = "geography", y = "estimate", fill = "metric", 
                                              bar_column = "bar", dec = 1,
                                              esttype = "percent", color = c("#91268F"), legend = FALSE, left_align='25%', bottom_padding=50)


candt_pop_change_chart_bar <- create_bar_chart(df = ofm_pop |> 
                                                filter(regional_geography == "Cities & Towns" & year >= base_year) |>
                                                group_by(geography) |>
                                                summarise(estimate = sum(total_change)/sum(total_population)) |>
                                                as_tibble() |> 
                                                arrange(estimate) |>
                                                mutate(metric = "Cities & Towns Pop Growth"),
                                              x = "geography", y = "estimate", fill = "metric", 
                                              bar_column = "bar", dec = 1,
                                              esttype = "percent", color = c("#91268F"), legend = FALSE, left_align='25%', bottom_padding=50)

# filtering for all cities and towns
allcities <- ofm_pop |>
  filter(filter == "4" & year >= base_year) |>
  group_by(geography) |>
  summarise(estimate = sum(total_change)/sum(total_population)) |>
  as_tibble()

# top 25 for estimate
top25 <- top_n(allcities, 25)

# top 25 for all cities and towns
pop_change_chart_bar <- create_bar_chart(df = top25 |> 
                                             arrange(estimate) |>
                                             mutate(metric = "All Cities & Towns Pop Growth"),
                                               x = "geography", y = "estimate", fill = "metric", 
                                               bar_column = "bar", dec = 1,
                                               esttype = "percent", color = c("#91268F"), legend = FALSE, left_align='25%', bottom_padding=50)

# for 2024 the share of the regional total by county
county_2024_pop_share_chart_bar <- create_bar_chart(df = ofm_pop |> 
                                                 filter(regional_geography == "County" & year == "2024") |>
                                                   filter(geography != "Region") |>
                                                 mutate(metric = "County Share of Regional Total for 2024"),
                                               x = "geography", y = "share_region_total", fill = "metric", 
                                               bar_column = "bar", dec = 1,
                                               esttype = "percent", color = c("#91268F"), legend = FALSE, left_align='25%', bottom_padding=50)

# for 2024 the share of the regional change by county
county_2024_pop_change_chart_bar <- create_bar_chart(df = ofm_pop |> 
                                                      filter(regional_geography == "County" & year == "2024") |>
                                                      filter(geography != "Region") |>
                                                      mutate(metric = "County Share of Population Change for 2024"),
                                                    x = "geography", y = "share_region_change", fill = "metric", 
                                                    bar_column = "bar", dec = 1,
                                                    esttype = "percent", color = c("#91268F"), legend = FALSE, left_align='25%', bottom_padding=50)


# share of the regional total by county since 2018
county_pop_share_chart <- create_bar_chart(df = ofm_pop |> 
                                                      filter(regional_geography == "County" & year >= base_year) |>
                                                      filter(geography != "Region") |>
                                             mutate(year = as.character(year), metric = "County Share of Regional Total Since 2018"),
                                                    x = "year", y = "share_region_total", fill = "geography", dec = 1,
                                                    esttype = "percent", color = c("#91268F", "#F05A28", "#8CC63E", "#00A7A0"), left_align='10%', bottom_padding=75)

# share of the regional change by county since 2018
county_pop_change_chart <- create_bar_chart(df = ofm_pop |> 
                                                       filter(regional_geography == "County" & year >= base_year) |>
                                                       filter(geography != "Region") |>
                                                      mutate(year = as.character(year), metric = "County Share of Population Change since 2018"),
                                                     x = "year", y = "share_region_change", fill = "geography", dec = 1,
                                                     esttype = "percent",  color = c("#91268F", "#F05A28", "#8CC63E", "#00A7A0"), left_align='10%', bottom_padding=75)
