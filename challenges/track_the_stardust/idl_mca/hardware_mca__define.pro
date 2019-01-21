; This file creates an EPICS_MCA object, disguised as a HARDWARE_MCA object.
; This is done so that the MCA_DISPLAY program can be device independent, and
; does not need to now if it is talking to an EPICS_MCA or a GENIE_MCA, etc.

; Each system will have a different version of this file, depending upon
; whether it is running EPICS, or Genie2k, etc.

function hardware_mca::init, record_name, environment_file=environment_file
    return, self->epics_mca::init(record_name, $
                                  environment_file=environment_file)
end

pro hardware_mca__define
hardware_mca = { hardware_mca, $
            INHERITS epics_mca}
end
