SELECT COUNT(PlayerID) as [number_of_players], Country
from PlayerDim pd join LocationDim on PlayerOriginID = LocationID
GROUP BY Country ORDER BY number_of_players desc 