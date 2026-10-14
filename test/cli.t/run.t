The demo account as CSV: a byte order mark, then the header and the September lines.

  $ statements demo -format csv | head -4 | tr -d '\r' | sed 's/^\xEF\xBB\xBF/<BOM>/'
  <BOM>entry_ref,booking_date,value_time,type,status,direction,amount,currency,balance,reversal,reference,description,counterparty,category,hold,authorised_at
  6.0,2026-09-01,2026-09-01T07:30:00Z,WITHDRAWAL,BOOKED,OUT,-980.00,EUR,1470.00,false,rent-09,Rent September,,,,
  8.0,2026-09-03,2026-09-03T02:00:00Z,CARD_SETTLEMENT,BOOKED,OUT,-4.80,EUR,1465.20,false,card-0001,Coffee Corner,,,card-0001,2026-09-02T12:14:00Z
  10.0,2026-09-08,2026-09-08T02:00:00Z,CARD_SETTLEMENT,BOOKED,OUT,-262.40,EUR,1202.80,false,card-0002,Grand Hotel,,,card-0002,2026-09-05T18:40:00Z

The Kyrgyz preamble comes before the header:

  $ statements demo -format csv -locale ky -preamble | head -2 | tr -d '\r' | sed 's/^\xEF\xBB\xBF//'
  Эсептен көчүрмө,,,,,,,,,,,,,,,
  Эсеп,customer:demo,,,,,,,,,,,,,,

The PDF starts with the version line and ends with the end-of-file marker:

  $ statements demo -locale ru -output statement.pdf
  $ head -c 8 statement.pdf; echo
  %PDF-1.4
  $ tail -c 6 statement.pdf
  %%EOF

An unknown language is refused:

  $ statements demo -locale pt 2>&1 | head -1
  Error parsing command line:
