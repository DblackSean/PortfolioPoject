SELECT eventName, COUNT(GameID) as [number of games], eventYear, MerchandiseType, sum(osf.MerchandiseSoldPND) as [total online merch sold GBP],SUM(ef.MerchandiseSoldPND) as [merch sold on site GBP] , SUM(TicketsSoldPND) as [tckets sold GBP], SUM(rf.TicketsRefundedPND) as [total tickets refunded GBP], SUM (rf.MerchandiseRefundedPND) as [merch refunded GBP], SUM (OnlineMerchantiseRefundedPND) as [online merch refunded GBP]
from GameFact gf JOIN EventDim ed on ed.EventID = gf.EventID join EventFact ef on ed.EventID = ef.EventID
join MerchandiseDim md on md.MerchandiseID = ef.MerchandiseID join OnlineSalesFact  osf on md.MerchandiseID = osf.MerchandiseID JOIN Refundfact rf on ef.ticketID = rf.ticketID 
GROUP by ROLLUP (EventName, EventYear, MerchandiseType);
