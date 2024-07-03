# Market monitoring

Signals are sent when month-to-month price increases reach a newly high level not seen in previous months.

## Source

The World Food Programme (WFP) produces a [Global Market Monitor dataset](https://www.wfp.org/publications/market-monitor) that monitors food prices in markets across a range of countries globally. The Global Market Monitor provides information on price changes for the most commonly consumed staples and their potential impacts on the cost of a basic food basket. For more details, please refer to the 'Methodology & Data' page at this [link](https://eur02.safelinks.protection.outlook.com/?url=https%3A%2F%2Fdataviz.vam.wfp.org%2Freports%2Fglobal-coverage-global-market-monitor-mar-2024%3F\_ga%3D2.216546710.1452952701.1719219215-2028703503.1710344334\&data=05%7C02%7Cseth.caldwell%40un.org%7C0263c631cc444c1fb32808dc95e733b0%7C0f9e35db544f4f60bdcc5ea416e6dc70%7C0%7C0%7C638550067449484289%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C0%7C%7C%7C\&sdata=%2B3wl7Pg0Cs6Z%2Fhb8%2FzE5hDdXV6dRDbhe%2BEqzzImWYMk%3D\&reserved=0). The Global Market Monitor follows a standardized methodology to calculate the impacts of price changes on the cost of a basic food basket for cross-country comparability. Global Market Monitor figures might differ from those presented in local market monitoring reports.

Subnational price data from a range of markets on core food basket items are aggregated to produce a national-level cost of food basket dataset.

Data is aggregated monthly, and when comparisons are able to be made with the previous month, the % price change at the national level is reported and classified. The change is classified as high if month-to-month changes is between 10 - 25% and severe if > 25%.

Access the data directly on [HDX](https://data.humdata.org/dataset/global-market-monitor) and find additional datasets and context on the [WFP website](https://www.wfp.org/publications/market-monitor).&#x20;

## Signals detection

We generate signals if we detect month-to-month change in a location classifed as high or severe and the previous month-to-month change was at a lower classification.

#### Examples:

[Libya](https://us14.campaign-archive.com/?e=0c9936e61d\&u=ea3f905d50ea939780139789d\&id=2fac00caa6#LBY): high change int he cost of hte food basket detected in early 2021.

[Syrian Arab Republic](https://us14.campaign-archive.com/?e=0c9936e61d\&u=ea3f905d50ea939780139789d\&id=bcb44b460e): severe change in the cost of the food basket detected in mid-2023.
