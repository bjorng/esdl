-ifndef(WOGGLE_HRL).
-define(WOGGLE_HRL,1).

-record(woggle_rect, 
	{
	  x,
	  y,
	  w,
	  h
	 }).

-record(woggle_res, 
	{
	  cdepth,
	  freq,
	  w,
	  h
	 }).
-endif. % WOGGLE_HRL
