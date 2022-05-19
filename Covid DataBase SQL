select * 
from PortfolioProjectCovid..['Covid Death$']
order by 3,4


--select *
--from PortfolioProjectCovid..CovidVacinations$
--order by 3,4

Select location, date, total_cases, new_cases, total_deaths, population
from PortfolioProjectCovid..['Covid Death$']
order by 1,2

--Looking at Total Cases vs Total Death
--Illustrates the likelyhood of dying if contracting Covid in the relative country
Select location, date, total_cases, total_deaths, (total_deaths/total_cases)*100 as DeathPercentage
from PortfolioProjectCovid..['Covid Death$']
where location like '%united kingdom%'
order by 1,2

--Total Cases vs Population
Select location, date, total_cases, population, (total_cases/population)*100 as PerentageOfPopulationInfected
from PortfolioProjectCovid..['Covid Death$']
where location like '%United kingdom%'
order by 1,2

--Looking at Countries with highest infection rate compared to Population
Select location,MAX(total_cases) as HighestInfectionCount, population, Max((total_cases/population))*100 as PerentageOfPopulationInfected
from PortfolioProjectCovid..['Covid Death$']
--where location like '%United kingdom%'
GROUP BY Location, Population
order by PerentageOfPopulationInfected desc

--Looking at Countries with the Highest Death Count per Population
Select location,MAX(cast(total_deaths as INT)) as TotalDeathCount
from PortfolioProjectCovid..['Covid Death$']
--where location like '%United kingdom%'
where continent is not null
GROUP BY Location
order by TotalDeathCount desc

--Looking at Countries with the Highest Death Count per Population - Percentage
Select location,MAX(cast((total_deaths/population)*100 as decimal(18,4))) as TotalDeathCount
from PortfolioProjectCovid..['Covid Death$']
--where location like '%United kingdom%'
where continent is not null
GROUP BY Location
order by TotalDeathCount asc

--LOOKING AT CONTINENTS
--Looking at Countries with the Highest Death Count per Population
Select location,MAX(cast(total_deaths as INT)) as TotalDeathCount
from PortfolioProjectCovid..['Covid Death$']
--where location like '%United kingdom%'
where continent is null
GROUP BY Location
order by TotalDeathCount desc

--
Select continent,MAX(cast(total_deaths as INT)) as TotalDeathCount
from PortfolioProjectCovid..['Covid Death$']
--where location like '%United kingdom%'
where continent is not null
GROUP BY continent
order by TotalDeathCount desc

--Global Numbers

SELECT date, sum(new_cases) as TotalCasesEachDay, SUM(cast(new_deaths as FLOAT)) as TotalDeaths, SUM(cast(new_deaths as float))/sum(new_cases)*100 as GlobalDeathPercentage
from PortfolioProjectCovid..['Covid Death$']
where continent is not null
--GROUP by [date]
order by 1,2

SELECT sum(new_cases) as TotalCasesEachDay, SUM(cast(new_deaths as FLOAT)) as TotalDeaths, SUM(cast(new_deaths as float))/sum(new_cases)*100 as GlobalDeathPercentage
from PortfolioProjectCovid..['Covid Death$']
where continent is not null
--GROUP by [date]
order by 1,2

-- Looking at Total Population vs Vaccination

SELECT dea.continent, dea. location, dea.date, dea.population, vac.new_vaccinations,
sum(cast(new_vaccinations as bigint)) over (PARTITION BY dea.[location] order by dea.location, dea.date) as RollingVaccinationTotal
from PortfolioProjectCovid..['Covid Death$'] dea
JOIN PortfolioProjectCovid..CovidVacinations$ vac
on dea.[location]=vac.[location] and dea.date =vac.[date]
Where dea.continent is not null --and dea.[location] like '%United Kingdom%'
order by 1,2,3


--USE CTE

With PopvsVac (continent, Location, date, population, new_vaccinations, RollingVaccinationTotal)
as
(
SELECT dea.continent, dea. location, dea.date, dea.population, vac.new_vaccinations,
sum(cast(new_vaccinations as bigint)) over (PARTITION BY dea.[location] order by dea.location, dea.date) as RollingVaccinationTotal
from PortfolioProjectCovid..['Covid Death$'] dea
JOIN PortfolioProjectCovid..CovidVacinations$ vac
on dea.[location]=vac.[location] and dea.date =vac.[date]
Where dea.continent is not null and dea.[location] like '%United Kingdom%'
--order by 1,2,3
)
SELECT *, (RollingVaccinationTotal/population)*100 as PercentageRunningTotal
from PopvsVac

--Create View to Store Later Visualisations

Create VIEW PercentPopulationVaccinated AS
SELECT dea.continent, dea. location, dea.date, dea.population, vac.new_vaccinations,
sum(cast(new_vaccinations as bigint)) over (PARTITION BY dea.[location] order by dea.location, dea.date) as RollingVaccinationTotal
from PortfolioProjectCovid..['Covid Death$'] dea
JOIN PortfolioProjectCovid..CovidVacinations$ vac
on dea.[location]=vac.[location] and dea.date =vac.[date]
Where dea.continent is not null --and dea.[location] like '%United Kingdom%'
--order by 1,2,3

