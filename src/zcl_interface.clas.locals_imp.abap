INTERFACE lif_partner.

  TYPES: BEGIN OF ts_attribute,
           name  TYPE string,
           value TYPE string,
         END OF ts_attribute,
         tt_attributes TYPE STANDARD TABLE OF ts_attribute WITH DEFAULT KEY.

  " Add table type for partners
  TYPES tt_partners TYPE STANDARD TABLE OF REF TO lif_partner WITH DEFAULT KEY.

  METHODS get_partner_attributes RETURNING VALUE(rt_result) TYPE tt_attributes.

ENDINTERFACE.

*----------------------------------------------------------------------*
* CLASS lcl_airline
*----------------------------------------------------------------------*
CLASS lcl_airline DEFINITION.
  PUBLIC SECTION.

    INTERFACES lif_partner.

    METHODS constructor
      IMPORTING
        iv_name           TYPE string OPTIONAL
        iv_contact_person TYPE string OPTIONAL
        iv_city           TYPE string OPTIONAL
          PREFERRED PARAMETER iv_name.

  PRIVATE SECTION.
    DATA name           TYPE string.
    DATA contact_person TYPE string.
    DATA city           TYPE string.
ENDCLASS.

CLASS lcl_airline IMPLEMENTATION.

  METHOD constructor.
    name = iv_name.
    contact_person = iv_contact_person.
    city = iv_city.
  ENDMETHOD.

  METHOD lif_partner~get_partner_attributes.
    rt_result = VALUE #(
      ( name = 'Type'           value = 'Airline' )
      ( name = 'Name'           value = name )
      ( name = 'Contact Person' value = contact_person )
      ( name = 'City'           value = city )
    ).
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_car_rental
*----------------------------------------------------------------------*
CLASS lcl_car_rental DEFINITION.
  PUBLIC SECTION.

    INTERFACES lif_partner.

    METHODS constructor
      IMPORTING
        iv_name           TYPE string OPTIONAL
        iv_contact_person TYPE string OPTIONAL
        iv_has_hgv        TYPE abap_bool OPTIONAL
          PREFERRED PARAMETER iv_name.

  PRIVATE SECTION.
    DATA name           TYPE string.
    DATA contact_person TYPE string.
    DATA has_hgv        TYPE abap_bool.
ENDCLASS.

CLASS lcl_car_rental IMPLEMENTATION.

  METHOD constructor.
    name           = iv_name.
    contact_person = iv_contact_person.
    has_hgv        = iv_has_hgv.
  ENDMETHOD.

  METHOD lif_partner~get_partner_attributes.
    rt_result = VALUE #(
      ( name = 'Type'           value = 'Car Rental' )
      ( name = 'Name'           value = name )
      ( name = 'Contact Person' value = contact_person )
      ( name = 'Has HGV'        value = COND #( WHEN has_hgv = abap_true THEN 'Yes' ELSE 'No' ) )
    ).
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_travel_agency
*----------------------------------------------------------------------*
CLASS lcl_travel_agency DEFINITION.
  PUBLIC SECTION.

    METHODS constructor.

    METHODS add_partner
      IMPORTING
        i_partner TYPE REF TO lif_partner.

    METHODS get_partners
      RETURNING VALUE(rt_result) TYPE lif_partner=>tt_partners.  " ← Use defined type

  PRIVATE SECTION.
    DATA partners TYPE lif_partner=>tt_partners.

ENDCLASS.

CLASS lcl_travel_agency IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD add_partner.
    APPEND i_partner TO partners.
  ENDMETHOD.

  METHOD get_partners.
    rt_result = partners.
  ENDMETHOD.

ENDCLASS.
