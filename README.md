# rsurveygizmo

This package allows you to pull data from SurveyGizmo directly from R using the API. This package adds the different domains from which data can be downloaded: US, EU or CA.

It builds on the initial work by [Derek Darves](https://derekyves.github.io/2016/06/06/rsurvey-gizmo.html) and [fowlerjk](https://github.com/fowlerjk/rsurveygizmo) who added the API secret introduced in later versions of the API.

For more info, visit the [Github repository](https://github.com/btlindert/rsurveygizmo) or install directly within R:

    library(devtools)
    install_github(repo="btlindert/rsurveygizmo")

Download regular survey data from Surveygizmo without email campaign data. See the function documentation for additional configurable parameters.

    your_survey <- "your_survey_id"
    your_api <- "your_api_key"
    your_secret <- "your_api_secret"
    your_locale <- "your_locale_here" # US, EU or CA

    # regular
    your_survey_data <- pullsg(your_survey, your_api, your_secret, your_locale)

    # completes only
    your_survey_data <- pullsg(your_survey, your_api, your_secret, your_locale, completes_only = TRUE)

Download all email campaign data for a particular survey.

    your_campaign_data <- pullsg_campaign(your_survey, your_api, your_secret, your_locale)

You can also combine the previous steps in one function. First, download the email campaign, then merge it, where possible, with the survey reponses.

    your_survey_with_campaign <- pullsg(your_survey, your_api, your_secret, your_locale, mergecampaign = TRUE)
