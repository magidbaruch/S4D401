@AbapCatalog.sqlViewName: 'ZS4D401_AIRPORT'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Read A List of Airports'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType: {
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view ZS4D401_AIRPORTS
  as select from /dmo/airport
{
  key airport_id as AirportID,
      name       as Name,
      city       as City,
      country    as Country
}
