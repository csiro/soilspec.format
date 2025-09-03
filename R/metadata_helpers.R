# Standard Metadata Fields

create.standard.metadata.container = function() {

  md <- list()
  md[['sample.id']] <- ''
  md[['spectra.id']] <- ''
  md[['spectra_source_file_name']] <- ''
  md[['date.time']] <- ''
  md[['response']] <- ''


  # DB Fields

  md[['instrument_technology_type']] <- ''
  md[['instrument_manufacturer']] <- ''
  md[['instrument_model']] <- ''
  md[['instrument_serial_number']] <- ''
  md[['instrument_resolution']] <- ''
  md[['instrument_min_wavelength']] <- ''
  md[['instrument_max_wavelength']] <- ''
  md[['instrument_units']] <- ''

  md[['spectra_mode']] <- ''
  md[['spectra_temperature']] <- ''
  md[['spectra_humidity']] <- ''
  md[['spectra_wavesignature_units']] <- ''


  return(md)
}
