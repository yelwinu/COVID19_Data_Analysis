#code to add to the markdown
global <- global %>% mutate(year = lubridate::year(date))

global_cases_per_pop <- global %>%
  group_by(Country_Region, year) %>%
  summarise(
    Total_Cases = max(cases, na.rm = TRUE),
    Total_Deaths = max(deaths, na.rm = TRUE),
    Population = max(Population, na.rm = TRUE)
  ) %>%
  mutate(Cases_Per_Population = 100* (Total_Cases / Population)) %>% # Compute cases per population
  ungroup()  

#filter out the countries with no population
no_population_data <- global_cases_per_pop %>% filter(Population == -Inf)
global_cases_per_pop <- global_cases_per_pop %>% filter(Population != -Inf)

# Get the top 5 countries overall
top_5_cases <- global_cases_per_pop %>%
  filter(year == 2023) %>% 
  arrange(desc(Cases_Per_Population)) %>%
  slice_head(n = 5)

colnames(top_5_cases) <- c("Country", "Year", "Number of Cases", "Population", "% of Cases Per Population")
kable(top_5_cases, caption = "Top 5 Countries with Most COVID-19 Cases")

#get top 5 countries and US data for all years 
top_5_countries_all_years <- global_cases_per_pop %>%
  filter(Country_Region %in% (top_5_cases$Country_Region) | Country_Region == "US")

#plot the top 5 countries and US data
ggplot(top_5_countries_all_years, aes(x = year, y = Cases_Per_Population, color = Country_Region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Countries with Most COVID-19 Cases Per Population and US",
       x = "Year", y = "% of Cases Per Population", color = "Country") +
  theme_minimal()

# Get the least 5 countries overall
least_5_cases <- global_cases_per_pop %>%
  filter(year == 2023) %>% 
  arrange(desc(Cases_Per_Population)) %>%
  slice_tail(n = 5)

colnames(least_5_cases) <- c("Country", "Year", "Number of Cases", "Population", "Percentage of Cases Per Population")
kable(least_5_cases, caption = "Top 5 Countries with Least COVID-19 Cases")

#get least 5 countries data for all years 
least_5_countries_all_years <- global_cases_per_pop %>%
  filter(Country_Region %in% (least_5_cases$Country_Region))

ggplot(least_5_countries_all_years, aes(x = year, y = Cases_Per_Population, color = Country_Region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Countries with Minimal COVID-19 Cases Per Population",
       x = "Year", y = "% Of Cases Per Population ", color = "Country") +
  theme_minimal()

#US data by state
US_by_state <- US %>% group_by(Province_State, Country_Region, date) %>% 
  summarize(cases = sum(cases),deaths = sum(deaths), Population = sum(Population)) %>%
  select(Province_State,Country_Region,date,cases,deaths,Population) %>% ungroup()

US_by_state <- US_by_state %>% mutate(year = lubridate::year(date))
US_state_cases_per_pop <- US_by_state %>%
  group_by(Province_State, year) %>%
  summarise(
    Total_Cases = max(cases, na.rm = TRUE),
    Total_Death = max(deaths, na.rm = TRUE),
    Population = max(Population, na.rm = TRUE)
  ) %>%
  mutate(Cases_Per_Population_Per = 100* (Total_Cases / Population)) %>% 
  ungroup()

#Guam is excluded since it is not a US state
US_state_cases_per_pop <- US_state_cases_per_pop %>% 
  filter(Population > 0 & Province_State != "Guam") %>% 
  filter(Province_State != "American Samoa" & Province_State != "Virgin Islands") %>% 
  filter(Province_State != "Northern Mariana Islands")

# Get the top 5 states overall
top_5_states <- US_state_cases_per_pop %>%
  filter(year == 2023) %>% 
  arrange(desc(Cases_Per_Population)) %>%
  slice_head(n = 5)

#get top 5 US states
top_5_states_all_years <- US_state_cases_per_pop %>%
  filter(Province_State %in% (top_5_states$Province_State))

#plot the top 5 US states
plot1 <- ggplot(top_5_states_all_years, aes(x = year, y = Cases_Per_Population, color = Province_State)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "5 US States with most COVID-19 Cases Per Population",
    x = "Year", y = "% of Cases Per Population", color = "States") +
  theme_minimal()

# Get the least 5 states overall
least_5_states <- US_state_cases_per_pop %>%
  filter(year == 2023) %>% 
  arrange(desc(Cases_Per_Population)) %>%
  slice_tail(n = 5)

#get least 5 US states
least_5_states_all_years <- US_state_cases_per_pop %>%
  filter(Province_State %in% (least_5_states$Province_State))


#plot the least 5 US states
plot2 <- ggplot(least_5_states_all_years, aes(x = year, y = Cases_Per_Population, color = Province_State)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "5 US States with least COVID-19 Cases Per Population",
       x = "Year", y = "% of Cases Per Population", color = "States") +
  theme_minimal()

#modeling 
US_state_totals <- US_by_state %>% 
  group_by(year) %>% 
  summarize(deaths = max(deaths), cases = max(cases),
            population = max(Population),
            deaths_per_case = deaths/cases)
global_totals <- global_cases_per_pop %>% 
  group_by(year) %>% 
  summarize(deaths = max(Total_Deaths), cases = max(Total_Cases),
            population = max(Population),
            deaths_per_case = deaths/cases)

combined_data <- rbind(US_state_totals, global_totals)
lm_model_total <- lm(deaths_per_case ~ year, data =combined_data )
summary(lm_model_total)
combined_data <- combined_data %>% mutate(pred = predict(lm_model_total))

# Plotting the data and the linear regression line
ggplot()+
  geom_line(data = US_state_totals, aes(x = year, y = deaths_per_case, color = "US States")) +
  geom_line(data = global_totals, aes(x = year,y = deaths_per_case,color = "Global"))+
  geom_line(data = combined_data, aes(x = year,y = pred,color = "Linear Regression Model"))+
  labs(
    title = "Trend of deaths in cases of COVID-19 over time",
    x = "Year",
    y = "Total Deaths/Total Cases"
  ) + theme_minimal()


#--------------------------------------------------------------------------------------------------------#
US_state_cases_per_pop <- US_state_cases_per_pop %>% mutate(deaths_per_case = Total_Death/Total_Cases)
US_state_cases_per_pop <- na.omit(US_state_cases_per_pop)
lm_model <- lm(deaths_per_case ~ year, data =US_state_cases_per_pop )
summary(lm_model)

US_state_cases_per_pop <- US_state_cases_per_pop %>% mutate(pred = predict(lm_model))


US_state_totals <- US_by_state %>% 
  group_by(year) %>% 
  summarize(deaths = max(deaths), cases = max(cases),
            population = max(Population),
            deaths_per_case = deaths/cases)
global_totals <- global_cases_per_pop %>% 
  group_by(year) %>% 
  summarize(deaths = max(Total_Deaths), cases = max(Total_Cases),
            population = max(Population),
            deaths_per_case = deaths/cases)

combined_data <- rbind(US_state_totals, global_totals)

lm_model_total <- lm(deaths_per_case ~ year, data =combined_data )
summary(lm_model_total)
combined_data <- combined_data %>% mutate(pred = predict(lm_model_total))
# Plotting the data and the linear regression line
ggplot()+
  geom_line(data = US_state_totals, aes(x = year, y = deaths_per_case, color = "US States")) +
  geom_line(data = global_totals, aes(x = year,y = deaths_per_case,color = "Global"))+
  geom_line(data = combined_data, aes(x = year,y = pred,color = "Linear Regression Model"))+
  labs(
    title = "Trend of deaths in cases of COVID-19 over time",
    x = "Year",
    y = "Total Deaths/Total Cases"
  ) + theme_minimal()

  ggplot(US_state_totals, aes(x = year)) +
  geom_line(aes(y = deaths_per_case, color = "Actual"), size = 1) +  # Actual data
  geom_line(aes(y = pred, color = "Predicted"), size = 1, linetype = "dashed") +  # Predicted data
  geom_point(aes(y = deaths_per_case, color = "Actual"), size = 3) +  # Points for actual data
  geom_point(aes(y = pred, color = "Predicted"), size = 3, shape = 1) +  # Points for predicted data
  labs(title = "Actual vs Predicted Deaths per Case",
       x = "Year", 
       y = "Deaths per Case",
       color = "Legend") +
  theme_minimal() 
  
US_totals %>% filter (cases >0) %>% ggplot(aes(x = date, y = cases)) +
  geom_line(aes(color = "cases")) + 
  geom_point(aes(color = "cases")) + 
  geom_line(aes(y = deaths , color = "deaths")) + 
  geom_point(aes(y = deaths, color = "deaths")) + 
  scale_y_log10() + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) + 
  labs(title = "cOVID19 in US", y = NULL)

selected_country <- "Afghanistan" 
country_time_series <- global_cases_per_pop %>%
  filter(Combined_Key == selected_country) %>%
  arrange(year)

country_time_series <-global %>% filter(Combined_Key == selected_country) %>% 
  filter(year == 2020) %>% arrange(year)



#US data by state
US_by_state <- US %>% group_by(Province_State, Country_Region, date) %>% summarize(cases = sum(cases),deaths = sum(deaths), 
                                                                                   Population = sum(Population)) %>% mutate(deaths_per_mill = deaths * 1000000 / Population) %>% 
  select(Province_State,Country_Region,date,cases,deaths,deaths_per_mill,Population) %>% ungroup()

US_by_state <- US_by_state %>% mutate(new_cases = cases - lag(cases), new_deaths = deaths - lag(deaths))

#US data in total
US_totals <- US_by_state %>% group_by(Country_Region, date) %>% summarize(cases = sum(cases),deaths = sum(deaths), 
                                                                          Population = sum(Population)) %>% mutate(deaths_per_mill = deaths * 1000000 / Population) %>% 
  select(Country_Region,date,cases,deaths,deaths_per_mill,Population) %>% ungroup()
