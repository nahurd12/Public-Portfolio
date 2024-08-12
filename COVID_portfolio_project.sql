SELECT * FROM vac
LIMIT 5;

SELECT * FROM deaths
LIMIT 5;

SELECT location, date, total_cases, new_cases, total_deaths, population
FROM deaths
ORDER BY 1,2;

-- Total Cases vs Total Deaths
SELECT location, date, total_cases, total_deaths, (total_deaths / total_cases)*100 AS death_percentage
FROM deaths
ORDER BY 1,2;

-- Look at Total Cases vs Population in United States
SELECT location, date, population, total_cases, (total_cases / population)*100 AS percent_population_infected
FROM deaths
WHERE location LIKE '%States%'
ORDER BY 1,2;

-- Look at countries with highest infection rate compared to population
SELECT location, population, MAX(total_cases) AS highest_infection_count, MAX((total_cases/population))*100 AS percent_population_infected
FROM deaths
GROUP BY location, population
ORDER BY percent_population_infected
DESC;

-- Showing countries with highest death count per population
SELECT location, MAX(cast(total_deaths AS INT)) AS total_death_count
FROM deaths
WHERE continent IS NOT NULL
GROUP BY location
ORDER BY total_death_count
DESC;

-- Break down by continent 
SELECT continent, MAX(cast(total_deaths AS INT)) AS total_death_count
FROM deaths
WHERE continent IS NOT NULL
GROUP BY continent
ORDER BY total_death_count
DESC;

-- Global numbers
SELECT SUM(new_cases) AS total_cases, SUM(new_deaths) AS total_deaths, SUM(new_deaths)/SUM(new_cases)*100 AS death_percentage
FROM deaths
WHERE continent IS NOT NULL 
ORDER BY 1,2;

-- JOIN death table and vaccine table 
SELECT * FROM deaths
JOIN vac
ON deaths.location = vac.location
AND deaths.date = vac.date

-- Look at total population vs vaccinations 
SELECT deaths.continent, deaths.location, deaths.date, deaths.population, vac.new_vaccinations
FROM deaths
JOIN vac
ON deaths.location = vac.location
AND deaths.date = vac.date
WHERE deaths.continent IS NOT NULL
ORDER BY 2,3

-- Creating a new colum to have a rolling count updating with each row
SELECT deaths.continent, deaths.location, deaths.date, deaths.population, vac.new_vaccinations, SUM(vac.new_vaccinations) 
OVER (PARTITION BY deaths.location ORDER BY deaths.location, deaths.date) AS rolling_people_vaccinated
FROM deaths
JOIN vac
ON deaths.location = vac.location
AND deaths.date = vac.date
WHERE deaths.continent IS NOT NULL
ORDER BY 2,3

-- USE CTE
WITH pop_vs_vac (continent, location, date, population, new_vaccinations, rolling_people_vaccinated)
AS 
(
SELECT deaths.continent, deaths.location, deaths.date, deaths.population, vac.new_vaccinations, SUM(vac.new_vaccinations) 
OVER (PARTITION BY deaths.location ORDER BY deaths.location, deaths.date) AS rolling_people_vaccinated
FROM deaths
JOIN vac
ON deaths.location = vac.location
AND deaths.date = vac.date
WHERE deaths.continent IS NOT NULL
)
SELECT *, (rolling_people_vaccinated/population)*100
FROM pop_vs_vac 

-- TEMP TABLE

DROP TABLE IF EXISTS percent_population_vaccinated
CREATE TABLE percent_population_vaccinated
(
	continent varchar(255), 
	location varchar(255),
	date date, 
	population numeric, 
	new_vaccinations numeric, 
	rolling_people_vaccinated numeric 
	)

INSERT INTO percent_population_vaccinated
SELECT deaths.continent, deaths.location, deaths.date, deaths.population, vac.new_vaccinations, SUM(vac.new_vaccinations) 
OVER (PARTITION BY deaths.location ORDER BY deaths.location, deaths.date) AS rolling_people_vaccinated
FROM deaths
JOIN vac
ON deaths.location = vac.location
AND deaths.date = vac.date
WHERE deaths.continent IS NOT NULL

SELECT *, (rolling_people_vaccinated/population)*100
FROM percent_population_vaccinated 

-- Creating VIEW to store data for visualizations 

CREATE VIEW percentpopulationvaccinated AS 

SELECT deaths.continent, deaths.location, deaths.date, deaths.population, vac.new_vaccinations, SUM(vac.new_vaccinations) 
OVER (PARTITION BY deaths.location ORDER BY deaths.location, deaths.date) AS rolling_people_vaccinated
FROM deaths
JOIN vac
ON deaths.location = vac.location
AND deaths.date = vac.date
WHERE deaths.continent IS NOT NULL

SELECT * FROM percentpopulationvaccinated
