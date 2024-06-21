# Market monitoring

Signals are sent when month-to-month price increases reach a newly high level not seen in previous months.

## Source

The World Food Programme (WFP) produces a global [Market Monitoring dataset](https://www.wfp.org/publications/market-monitor) that monitors food prices in markets across a range of countries globally. Subnational price data from a range of markets on core food basket items are aggregated to produce a national-level cost of food basket dataset.

Data is aggregated monthly, and when comparisons are able to be made with the previous month, the % price change at the national level is reported and classified. The change is classified as high if month-to-month changes is between 10 - 25% and severe if > 25%.

Access the data directly on [HDX](https://data.humdata.org/dataset/global-market-monitor) and find additional datasets and context on the [WFP website](https://www.wfp.org/publications/market-monitor).&#x20;

## Signals detection

We generate signals if we detect month-to-month change in a location classifed as high or severe and the previous month-to-month change was at a lower classification.

#### Examples:

[Libya](https://us14.campaign-archive.com/?e=0c9936e61d\&u=ea3f905d50ea939780139789d\&id=2fac00caa6#LBY): high change int he cost of hte food basket detected in early 2021.

[Syrian Arab Republic](https://us14.campaign-archive.com/?e=0c9936e61d\&u=ea3f905d50ea939780139789d\&id=bcb44b460e): severe change in the cost of the food basket detected in mid-2023.
