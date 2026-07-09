# HDX Signals — Architecture

Four diagrams covering the system from different angles.

---

## 1. System Overview

High-level flow from data sources through signal generation to final outputs.

```mermaid
flowchart LR
    subgraph external["External Data Sources"]
        direction TB
        A["ACLED (conflict events)"]
        B["IDMC (displacement)"]
        C["IPC (food insecurity)"]
        D["WHO (cholera)"]
        E["ACAPS (INFORM severity)"]
        F["JRC (agri hotspots)"]
        G["WFP (market monitor)"]
    end

    subgraph indicators["src/indicators/ — 8 modules<br/>GitHub Actions @ 7 AM UTC weekdays"]
        IND["raw → wrangle → alert<br/>summary → plot → map"]
    end

    AZ[("Azure Blob Storage<br/>output/{indicator}/signals.parquet")]

    subgraph triage["Manual Triage<br/>triage_signals.yaml (workflow_dispatch)"]
        CMD{"USER_COMMAND"}
    end

    MC["Mailchimp<br/>Subscriber email + Archive"]
    HDX[("HDX<br/>signals.csv")]
    SLACK["Slack<br/>daily digest @ 8 AM UTC"]

    external --> indicators
    indicators -->|"generate_signals.R / create_campaigns.R"| AZ
    AZ --> CMD
    CMD -->|APPROVE| MC
    CMD -->|APPROVE| HDX
    CMD -->|DELETE — reruns fresh| indicators
    CMD -->|ARCHIVE — no email| AZ
    AZ -->|"check_signals_updates.R"| SLACK
```

---

## 2. Indicator Module Pipeline

How a single indicator module processes raw data into a Mailchimp campaign. Every indicator follows this same structure — the file names are `{step}_{indicator}.R` inside `src/indicators/{indicator}/utils/`.

```mermaid
flowchart TD
    EXT["External API / Source"]

    RAW["raw()<br/>Fetch from external source"]
    WR["wrangle()<br/>Clean, standardize, aggregate by location-date"]
    AL["alert()<br/>Detect threshold crossings — set alert_level_numeric"]
    FA["filter_alerts()<br/>Suppress if same location alerted within 60 or 180 days"]
    GA["generate_alerts()<br/>Attach location metadata (ISO3, region, population)"]

    subgraph content["generate_campaign_content()"]
        SUM["summary()<br/>Contextual narrative text"]
        PLT["plot()<br/>Time-series chart"]
        MAP["map()<br/>Country-level choropleth"]
        INF["info()<br/>HDX links, source URLs, metadata"]
    end

    subgraph campaigns["create_campaigns.R"]
        ARC["Archive campaign<br/>No segmentation — sent to HDX internal email"]
        SUB["Subscriber campaign<br/>Conditional merge tags per location<br/>Custom static segment (location + indicator interest)"]
    end

    AZ[("Azure Blob<br/>output/{indicator}/signals.parquet<br/>(pending triage)")]

    EXT --> RAW --> WR --> AL --> FA --> GA --> content
    content --> campaigns
    campaigns --> AZ
```

---

## 3. Signal Lifecycle

State transitions from alert detection through human triage to final disposition. Signals sit in pending state until a human runs the triage workflow.

```mermaid
stateDiagram-v2
    [*] --> Monitoring : Daily run (GitHub Actions)

    Monitoring --> Quiet : No threshold crossed
    Quiet --> Monitoring : Next scheduled run

    Monitoring --> PendingTriage : Alert detected — campaign created in Mailchimp

    PendingTriage --> Approved : APPROVE — emails sent, saved to signals.parquet, pushed to HDX
    PendingTriage --> Deleted : DELETE — campaign wiped, indicator reruns from scratch
    PendingTriage --> Archived : ARCHIVE — campaign deleted, alert record kept, no email sent

    Approved --> [*]
    Archived --> [*]
    Deleted --> Monitoring : Picks up next cycle
```

---

## 4. GitHub Actions Schedule

When each workflow runs and what triggers it.

```mermaid
flowchart TD
    subgraph daily["Scheduled — Weekdays Mon–Fri UTC"]
        subgraph t7["7:00 AM — Monitor workflows (run in parallel)"]
            M1["monitor_acled_conflict"]
            M2["monitor_idmc_displacement_conflict"]
            M3["monitor_idmc_displacement_disaster"]
            M4["monitor_ipc_food_insecurity"]
            M5["monitor_who_cholera"]
            M6["monitor_acaps_inform_severity"]
            M7["monitor_jrc_agricultural_hotspots"]
            M8["monitor_wfp_market_monitor"]
        end

        subgraph t8["8:00 AM — Slack digest"]
            SLACK["post_slack_update<br/>check_signals_updates.R"]
        end
    end

    subgraph weekly["Scheduled — Mondays @ 8:00 AM UTC"]
        BACKUP["backup_mailchimp<br/>Snapshot audience to Azure"]
        ANALYTICS["user_audience_analysis<br/>Create user + campaign analytics datasets"]
    end

    subgraph manual["Manual — workflow_dispatch only"]
        TRIAGE["triage_signals<br/>Inputs: USER_COMMAND · INDICATOR_ID · confirm flag"]
        STATIC["Static asset updates<br/>update_locations_data · update_indicator_mapping<br/>update_data_dictionary · update_acled_info · update_asap_codes"]
    end

    subgraph automated["Automated — on pull request to main"]
        CHECK["check_changes — validates CHANGES.md is updated"]
        LINT["lint — R linting (lintr)"]
        TEST["test_signals — unit tests for email components + utils"]
        PROMPTS["publish_prompts — rebuilds docs/PROMPTS.md if prompt .txt files changed"]
    end

    t7 --> t8
```

> **Note on dry-run defaults:** All monitor workflows default to `HS_DRY_RUN: TRUE` and `HS_LOCAL: TRUE`. Production writes only happen when these are explicitly set to `FALSE` in the workflow dispatch inputs. The triage workflow similarly gates on `HS_DRY_RUN` before dispatching emails or writing to HDX.

---

## 5. Static Assets

Static assets are reference data stored in Azure Blob `input/`. They are updated infrequently via manual `workflow_dispatch` and consumed by the main signal pipeline at runtime.

```mermaid
flowchart LR
    subgraph ext["External / Manual Sources"]
        direction TB
        HDX_S["HDX API"]
        ACLED_S["ACLED API"]
        JRC_S["JRC / FAO CSV"]
        UN_S["UN Geo Hub<br/>(admin boundaries)"]
        MC_S["Mailchimp config"]
    end

    subgraph scripts["src-static/ scripts<br/>(manual workflow_dispatch)"]
        direction TB
        ULD["update_locations_data<br/>locations → adm0 → centroids<br/>cities → HRP → map settings → metadata"]
        UAI["update_acled_info"]
        UAS["update_asap_codes"]
        UIM["update_indicator_mapping"]
        UDD["update_data_dictionary"]
    end

    subgraph azure["Azure Blob — input/"]
        direction TB
        LOC["locations.parquet<br/>covered ISO3 codes"]
        META["locations_metadata.parquet<br/>names, regions, centroids"]
        GEO["adm0/ · centroids/ · cities/<br/>spatial files for maps"]
        HRP["hrp_locations.parquet<br/>HRP country list"]
        IND_MAP["indicator_mapping.parquet<br/>Mailchimp IDs, email subjects"]
        ACLED_I["acled_info.parquet<br/>coverage dates + location URLs"]
        ASAP["asap_iso3.parquet<br/>ASAP0 → ISO3 mapping"]
    end

    subgraph consumes["Main pipeline — where each asset is used"]
        direction TB
        C1["raw()<br/>reads acled_info, asap_iso3"]
        C2["generate_alerts()<br/>reads locations, locations_metadata, hrp"]
        C3["map() functions<br/>reads adm0, centroids, cities"]
        C4["create_campaigns()<br/>reads indicator_mapping"]
    end

    HDX_S & UN_S --> ULD
    ACLED_S --> UAI
    JRC_S --> UAS
    MC_S --> UIM

    ULD --> LOC & META & GEO & HRP
    UAI --> ACLED_I
    UAS --> ASAP
    UIM --> IND_MAP
    UDD --> azure

    LOC & META & HRP --> C2
    ACLED_I & ASAP --> C1
    GEO --> C3
    IND_MAP --> C4
```
