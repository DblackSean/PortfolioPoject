--Number of Games played at each event
SELECT 
eventName, 
COUNT(GameID) as [number of games], 
eventYear, 
MerchandiseType, 
Format (SUM(TicketsSoldPND), 'c', 'en-GB') as [tickets sold GBP], 
Format (sum (osf.MerchandiseSold), 'c', 'en-GB') as [total merch sold online GBP], 
Format (SUM(ef.MerchandiseSoldPND), 'c', 'en-GB') as [merch sold on site GBP]
from GameFact gf 
JOIN EventDim ed on ed.EventID = gf.EventID 
join EventFact ef on ed.EventID = ef.EventID
join MerchandiseDim md on md.MerchandiseID = ef.MerchandiseID 
join OnlineSalesFact  osf on md.MerchandiseID = osf.MerchandiseID
GROUP by EventYear, EventName, MerchandiseType 
ORDER by eventName, eventYear asc

/*By running the above query, we can retrieve the number of games played at each event, with the details of the ticket, merchandise, and online merchandise sales, grouped by event year and merchandise type. 
With such a query we can run correlations between the number of games and the number of tickets sold. 
This might indicate if it is worth having more games for an event to attract more spectators (ticket buyers). 
The business can also analyse and visualise which are the most successful merchandise type per event every year. 
This can be plotted on a line chart to analyse the recent trends in merchandise sales.*/

--Number of players belonging to each gender per country
select 
Country, 
playergender as [Gender of player],
cnt as [Gender Breakdown], 
SUM(cnt) over (partition by country) as [Total Players from Country]
from (select 
            ld.Country,
            pdm.playergender,
            count(PlayerGender) as cnt
            from PlayerDim pdm, LocationDim ld 
            where PlayerOriginID = LocationID
            GROUP by PlayerGender, ld.Country) s
GROUP by Country, PlayerGender, cnt

/*The above query retrieves the number of players belonging to each gender categorisation per country, as well as the total number of players from each country.
The organisation might want to run such a query to run some analysis on inclusivity. 
This information can direct the business in making strategic decisions to increase participation by both players and spectators from wider demographics. 
Alternatively, this information can be used to assist in deciding which countries are worth investing in in terms of promotion or player recruitment.*/

--mechandise sales
select 
eventName, 
EventYear, 
ProviderName,
Format (sum(ef.MerchandiseSoldPND), 'c', 'en-GB') as [Merchandise Sold at Event GBP],
Format (sum(osl.MerchandiseSoldPND),'c', 'en-GB')as [Merchandise Sold Online GBP], 
Format (sum(MerchandiseRefundedPND),'c', 'en-GB')as [Merchandise Refund Event Sales GBP], 
Format (sum(OnlineMerchantiseRefundedPND),'c', 'en-GB') [Merchandise Online Sales GBP]
from 
EventDim ed join eventfact ef 
on ed.EventID = ef.EventID 
join MerchandiseDim md 
on ef.MerchandiseID = md.MerchandiseID 
join ProviderDim pd 
on md.MerchandiseProviderID = pd.ProviderID 
join OnlineSalesFact osl 
on md.MerchandiseID = osl.MerchandiseID 
join RefundFact rf 
on md.MerchandiseID = rf.MerchandiseID
GROUP by ProviderName, EventName, EventYear
HAVING SUM(MerchandiseRefundedPND) > AVG(MerchandiseRefundedPND) 
and SUM(OnlineMerchantiseRefundedPND) > AVG(OnlineMerchantiseRefundedPND)

/*The above query retrieves the merchandise sales and refunds for both online and on-site sales at the various events, per merchandise provider, event name and event provider.
With such a query the business can have an initial assessment of which providers are causing the most total refunds and relative refunds. 
This can be used as an initial quality assessment which can assist in indicating which providers are having the highest data quality issues.
This query demonstrated the integrated nature of the data warehouse as because of the use of modelling dimensions we can access multiple connected dimensions.*/

--promotion types
select 
promotiontype, 
marketeerName, 
pd.promotionduration as [planned promotion duration], 
ef.PromotionDuration as [actual promotion duration], 
format(promotioncost, 'c', 'en-GB') as [Promotion Cost],
format(promotionrevenue, 'c', 'en-GB') as [Promotion Revenue],
format (promotionrevenue - promotioncost, 'c', 'en-GB') as [Profit/Loss],
Completedpromotiondur = case when pd.PromotionDuration < ef.PromotionDuration then 'Yes' else 'No' end
FROM PromotionDim pd JOIN EventFact ef on pd.promotionID = ef.PromotionID JOIN MarketeerDim on pd.MarketeerID = MarketeerDim.MarketeerID
WHERE promotionrevenue - promotioncost < 0 
ORDER BY[Profit/Loss] desc

/*The above contains all the promotion types run during all the events, where the promotion run at a loss. 
I also included a column where we can easily filter if a promotion lasted the entire planned duration or if it was cut short.
The business can use this query to analyse correlations between promotion losses and promotions durations. 
The business can also easily visualise which promotion type and marketeers have had the most losses.*/

--Champion stats
SELECT ChampionItemName, MAX(prkills) as [Highest Kill], max(prassists) as [highest assist], max(prdeaths) as [highest death]
from ChampionItemDim cid JOIN ChampionInGameSpecDim cigs 
on cid.ChampionItemID=cigs.ChampionItemID inner JOIN PlayerInGameDim pig 
on cigs.PlayerInGameID = pig.PlayerInGameID join PersonalRecordDim prd 
on pig.PRID = prd.PRID
GROUP by ChampionItemName

/*The above query retrieves the highest kill count, assist count and death count for each champion item. 
The business would run such a query to investigate any game imbalances such as weapons or items being too powerful which can negatively affect the players' experience.*/
