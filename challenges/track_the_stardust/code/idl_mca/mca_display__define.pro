;   MCA_DISPLAY
;
;   This program provides a multichannel analyzer (MCA) display using
;   IDL widgets. It emulates the look and feel of the Canberra MCA
;   package on the VAX.
;
;   Mark Rivers
;   CARS
;   University of Chicago
;   5640 S. Ellis Ave.
;   Chicago, IL 60632
;   Phone:  312-702-2279
;   FAX:    312-702-5454
;   e-mail  rivers@cars.uchicago.edu
;
;   This program requires that the following environment variables be set:
;       MCA_PREFERENCES   - File name to save/restore for MCA program
;                           preferences, such as size, colors, line style,
;                           etc.  If environment variable not set then the
;                           file mca.preferences in the current default
;                           directory is used
;       MCA_HELP_COMMAND  - System command to display MCA help text
;       XRF_PEAK_LIBRARY  - File containing XRF line data.
;                           Used by lookup_xrf_line.pro
;
;   This program requires at least version 3.0 of the MCA record, which
;   supports fields for ROI labels and calibration parameters.
;
;   Modifications:
;       (No release notes made in this file until Version 4.3)
;       MLR     5/14/99     Version 4.3.  Fixed logic for counts/second
;       MLR     5/15/99     Version 4.3.1 Fixed bug when opening invalid file
;                           Fixed statistics updates and marker repair when
;                           displaying ROIs.
;       MLR     5/17/99     Version 4.3.2 Improved centroid display on very
;                           sharp peaks.
;                           show_stats  when markers are moved by typing
;                           into text widget.
;       MLR     6/60/99     Version 4.3.3 Added autorestart option
;       MLR     2/7/00      Version 4.3.4 Fixed syntax error in case statement
;       MLR     2/13/00     Version 4.3.5 Fixed autoscale problem in log mode
;                                         Fixed divide by zero error in fitting
;                                         when right marker=left marker + 1
;       MLR     4/7/00      Version 4.3.6 Use new object version of JCPDS,
;                           correct for temperature as well as pressure.
;       MLR     5/22/00     Version 4.3.7 Delete JCPDS object before reading
;                           file to start with all new values.
;       MLR     5/31/00     Version 4.3.8 Fixed bug where closing background
;                           spectrum with JCPDS window open would cause
;                           infinite error loop next time a JCPDS file was
;                           read.
;       MLR     10/18/00    Version 4.3.9 Check limits on self.display.hmin, hmax
;       MLR     11/27/00    Version 4.3.10 Read in aligned total for MED files
;       MLR     11/29/00    Version 4.3.11 Fixed bug introduced in 4.3.10, fixed
;                           bug in CPS calculation, fixed bug when MCA objects not
;                           valid.
;       MLR     1/05/01     Version 4.3.12 Added support for spreadsheet output
;                           in peak fitting.
;       MLR     1/05/01     Version 4.3.13 Write and read background structure to/from
;                           peaks file.  Uses new version of read_peaks.pro and
;                           write_peaks.pro
;       MLR     2/14/01     Version 4.3.14 Fixed bug where filename was not updated.
;                           Fixed title on plots when file not yet saved.  Moved plot
;                           legend from upper right to upper left so file name fits.
;                           Fixed bug in logic checking acquire status with /check_new
;       MLR     3/20/01     Version 4.3.15 Trim leading and trailing blanks from ROI labels
;                           so that x-ray lines and gamma lines are recognized when user
;                           forgets to delete leading blanks.
;                           MCA__DEFINE.PRO was changed to increase MAX_CHANS from
;                           4096 to 8192.
;       MLR     7/18/01     Version 4.3.16 Fixed bug which caused energy calibration
;                           and ROIs to be incorrect when reading MED spectra for
;                           any but the first detector.
;       MLR    11/21/01     Version 4.3.17  Fixed problem with IDL 5.5 in
;                           peak fits looking up XRF lines
;



;*****************************************************************************
function mca_display_get_fonts, font_size=font_size

    if (n_elements(font_size) eq 0) then font_size=1
    font_size = (font_size > 0) < 3
    fonts = {mca_display_fonts,         $
        default:        get_font_name ( /helvetica, size=font_size),    $
        menu:           get_font_name ( /bold,      size=font_size),    $
        big_menu:       get_font_name ( /bold,      size=font_size+1),  $
        label:          get_font_name ( /bold,      size=font_size),    $
        big_label:      get_font_name ( /bold,      size=font_size+1),  $
        button:         get_font_name (             size=font_size),    $
        big_button:     get_font_name (             size=font_size+1),  $
        text:           get_font_name (             size=font_size),    $
        big_text:       get_font_name (             size=font_size+1)   $
    }
    return, fonts
end



;*****************************************************************************
pro mca_display::file_options
    desc = [                                                                  $
        '1, BASE,   ,ROW, FRAME',                                     $
        '2, BUTTON,  No|Yes, EXCLUSIVE, ROW, ' +                        $
                    'LABEL_LEFT =Warning when overwriting file:, ' + $
                    'TAG=warn_overwrite,' +             $
                    'SET_VALUE ='+string(self.options.warn_overwrite),    $
        '1, BASE,   ,ROW, FRAME',                                     $
        '2, BUTTON,  No|Yes, EXCLUSIVE, ROW, ' +                        $
                    'LABEL_LEFT =Warning when erasing unsaved data:, ' + $
                    'TAG=warn_erase,' +             $
                    'SET_VALUE ='+string(self.options.warn_erase),    $
        '1, BASE,   ,ROW, FRAME',                                     $
        '2, BUTTON,  No|Yes, EXCLUSIVE, ROW, ' +                        $
                    'LABEL_LEFT =Informational popup after saving file:, ' + $
                    'TAG=inform_save,' +             $
                    'SET_VALUE ='+string(self.options.inform_save),    $
        '1, BASE,   ,ROW, FRAME',                                     $
        '2, BUTTON,  No|Yes, EXCLUSIVE, ROW, ' +                        $
                    'LABEL_LEFT =Autosave when acquisition stops:, ' + $
                    'TAG=autosave,' +             $
                    'SET_VALUE ='+string(self.options.autosave),    $
        '1, BASE,   ,ROW, FRAME',                                     $
        '2, BUTTON,  No|Yes, EXCLUSIVE, ROW, ' +                        $
                    'LABEL_LEFT =Auto-restart when acquisition stops:, ' + $
                    'TAG=autorestart,' +             $
                    'SET_VALUE ='+string(self.options.autorestart),    $
        '1, BASE,   ,ROW, ',                                     $
        '0, BUTTON,  OK, QUIT, TAG =ok',                                $
        '2, BUTTON,  Cancel, QUIT, TAG =cancel']

    form = cw_form( /column, title='MCA File Options', desc)

    if (not form.ok) then return
    self.options.warn_overwrite = form.warn_overwrite
    self.options.warn_erase = form.warn_erase
    self.options.inform_save = form.inform_save
    self.options.autosave = form.autosave
    self.options.autorestart = form.autorestart
end


;*****************************************************************************
pro mca_display::save_prefs

    ; Read the current color palette
    ncolors = n_tags(self.colors)
    tvlct, red, green, blue, /get
    self.palette.red   = red[0:ncolors-1]
    self.palette.green = green[0:ncolors-1]
    self.palette.blue  = blue[0:ncolors-1]

    preferences = {                                            $
        mca_filepath:           self.file.filepath,            $
        mca_filename:           self.file.filename,            $
        mca_name:               self.file.mca_name,            $
        display_vlog:           self.display.vlog,             $
        display_vauto:          self.display.vauto,            $
        display_psym:           self.display.psym,             $
        palette:                self.palette,                  $
        display_update_time:    self.display.update_time,      $
        fonts:                  self.fonts,                    $
        options_autosave:       self.options.autosave,         $
        options_inform_save:    self.options.inform_save,      $
        options_warn_overwrite: self.options.warn_overwrite,   $
        options_warn_erase:     self.options.warn_erase,       $
        options_download_rois:  self.options.download_rois,    $
        options_download_cal:   self.options.download_cal,     $
        options_download_data:  self.options.download_data,    $
        print:                  self.print                     $
    }

    catch, ioerror
    if (ioerror ne 0) then begin
        t = dialog_message(/error, dialog=self.widgets.base, $
            ['Error writing preferences file: ' + self.file.preffile, $
            !err_string])
        return
    endif
    save, /xdr, file = self.file.preffile, preferences
end


;*****************************************************************************
pro mca_display::restore_prefs

    catch, ioerror
    if (ioerror ne 0) then begin
        t = dialog_message(/error, $
            ['Error reading preferences file: ' + $
             self.file.preffile + '.  Using defaults', $
             !err_string])
        return
    endif
    restore, file = self.file.preffile
    self.file.filepath          = preferences.mca_filepath
    self.file.filename          = preferences.mca_filename
    self.file.mca_name          = preferences.mca_name
    self.display.vlog           = preferences.display_vlog
    self.display.vauto          = preferences.display_vauto
    self.display.psym           = preferences.display_psym
    self.palette                = preferences.palette
    self.display.update_time    = preferences.display_update_time
    self.fonts                  = preferences.fonts
    self.options.autosave       = preferences.options_autosave
    self.options.inform_save    = preferences.options_inform_save
    self.options.warn_overwrite = preferences.options_warn_overwrite
    self.options.warn_erase     = preferences.options_warn_erase
    self.options.download_rois  = preferences.options_download_rois
    self.options.download_cal   = preferences.options_download_cal
    self.options.download_data  = preferences.options_download_data
    self.print                  = preferences.print
end


;*****************************************************************************
pro mca_display_cleanup, dying_id ; called after widget hierarchy destroyed

    widget_control, dying_id, get_uvalue=mca_display
    ; The following causes error"
    ; OBJ_DESTROY: Attempt to destroy an object within its INIT method:
    ;              <ObjHeapVar4(MCA_DISPLAY)>.
    ; on IDL 6.0 VM on Linux.
    ; obj_destroy, mca_display
    ; Instead call new function, cleanup1 directly
    mca_display->cleanup1
end


;*****************************************************************************
pro mca_display::cleanup1

    wdelete, self.windows.pixmap
    self->save_prefs
    obj_destroy, self.foreground.mca
    obj_destroy, self.background.mca
end

;*****************************************************************************
pro mca_display::cleanup
    self->cleanup1
end


;*****************************************************************************
function mca_display::chan_to_cal, chan

    case self.display.horiz_mode of
    0: return, chan
    1: return, self.foreground.mca->chan_to_energy(chan)
    2: return, self.foreground.mca->chan_to_d(chan)
    endcase
end

;*****************************************************************************
function mca_display::cal_to_chan, cal

    case self.display.horiz_mode of
    0: return, cal                                        ; Channel
    1: return, self.foreground.mca->energy_to_chan(cal)   ; Energy
    2: return, self.foreground.mca->d_to_chan(cal)        ; d-spacing
    endcase
end



;*****************************************************************************
pro mca_display::new_inputs

    if (not self.foreground.valid) then self.foreground.data = 0L
    if (not self.background.valid) then self.background.data = 0L

    if (self.foreground.valid) then sensitive=1 else sensitive=0
    widget_control, self.widgets.add_roi, sensitive=sensitive
    widget_control, self.widgets.del_roi, sensitive=sensitive
    widget_control, self.widgets.clear_roi, sensitive=sensitive
    widget_control, self.widgets.label_roi, sensitive=sensitive
    widget_control, self.widgets.next_roi, sensitive=sensitive
    widget_control, self.widgets.prev_roi, sensitive=sensitive
    widget_control, self.widgets.klm_label, sensitive=sensitive
    widget_control, self.widgets.klm_down, sensitive=sensitive
    widget_control, self.widgets.klm_up, sensitive=sensitive
    widget_control, self.widgets.save_next, sensitive=sensitive
    widget_control, self.widgets.save_as, sensitive=sensitive
    widget_control, self.widgets.print, sensitive=sensitive
    widget_control, self.widgets.calibrate_energy, sensitive=sensitive
    widget_control, self.widgets.calibrate_two_theta, sensitive=sensitive

    widget_control, self.widgets.display_jcpds, sensitive=sensitive
    widget_control, self.widgets.fit_peaks, sensitive=sensitive

    if self.foreground.is_detector then sensitive=1 else sensitive=0
    widget_control, self.widgets.on, sensitive=sensitive
    widget_control, self.widgets.off, sensitive=sensitive
    widget_control, self.widgets.erase, sensitive=sensitive
    widget_control, self.widgets.elive, sensitive=sensitive
    widget_control, self.widgets.ereal, sensitive=sensitive
    widget_control, self.widgets.presets, sensitive=sensitive

    if (self.foreground.valid) and (self.background.valid) then $
            sensitive=1 else sensitive=0
    widget_control, self.widgets.swap_foreground, sensitive=sensitive

    title = 'MCA Display Version ' + self.options.version + ' '
    if self.foreground.valid then $
        title = title + '(Foreground=' + self.foreground.name + ') '
    if self.background.valid then $
        title = title + '(Background=' + self.background.name + ') '
    widget_control, self.widgets.base, tlb_set_title = title
end


;*****************************************************************************
pro mca_display::open_det, background=background

    background = keyword_set(background)
    geometry=widget_info(self.widgets.base, /geometry)
    x = geometry.xoffset + geometry.xsize/2
    y = geometry.yoffset + geometry.ysize/2
    base = widget_base( title = 'Open detector  ', $
                        group = self.widgets.base, /column, $
                        xoffset=x, yoffset=y)
    widgets = {name: 0L, ok: 0L, cancel: 0L}
    widgets.name = cw_field(base, value=self.file.mca_name, /column, $
                    /return_events, title='Detector name')
    row = widget_base(base, /row)
    widgets.ok = widget_button(row, value='OK', font=self.fonts.big_button)
    widgets.cancel = widget_button(row, value='Cancel', $
                     font=self.fonts.big_button)

    widget_control, base, set_uvalue = $
            {mca_display: self, background: background, widgets: widgets}
    widget_control, base, /realize
    xmanager, 'mca_display::open_det', base, $
                event='mca_display_open_det_event', /no_block

end

;*****************************************************************************
pro mca_display_open_det_event, event
    ; called when Open detector form is modified

    widget_control, event.top, get_uvalue = top
    top.mca_display->open_det_event, event, top.background, top.widgets
end

;*****************************************************************************
pro mca_display::open_det_event, event, background, widgets

    if (event.id eq widgets.name) or (event.id eq widgets.ok) then begin
        widget_control, widgets.name, get_value=name
        widget_control, /hourglass
        self->open_detector, name, background=background
    endif

    widget_control, event.top, /destroy
end


;*****************************************************************************
pro mca_display::open_detector, name, background=background

    mca = obj_new('hardware_mca', name)
    if (obj_valid(mca)) then begin
        if (keyword_set(background)) then begin
            obj_destroy, self.background.mca
        endif else begin
            obj_destroy, self.foreground.mca
        endelse
        self->open, mca, name, background=background
    endif else begin
        ok = dialog_message( /error, dialog = self.widgets.base, $
                'Unable to open detector ' + name + '.')
        if (keyword_set(background)) then begin
            self.background.valid = 0
            self.background.is_detector = 0
        endif else begin
            self.foreground.valid = 0
            self.foreground.is_detector = 0
        endelse
        self->new_inputs
    endelse
end


;*****************************************************************************
pro mca_display::select_det, n_det, detector

    geometry=widget_info(self.widgets.base, /geometry)
    x = geometry.xoffset + geometry.xsize/2
    y = geometry.yoffset + geometry.ysize/2
    base = widget_base( title = 'Select detector  ', xoffset=x, yoffset=y, $
                        group = self.widgets.base, /row, /modal)
    widgets = {label: 0L, list: 0L}
    widgets.label = widget_label(base, value='Select detector to display:')
    choices = ['Det. '+strtrim(indgen(n_det) + 1, 2), 'Total']
    pdetector = ptr_new(detector)
    widgets.list = widget_droplist(base, value=choices, uvalue=pdetector)
    widget_control, base, set_uvalue = $
            {mca_display: self, detector: 0L, widgets: widgets}
    widget_control, base, /realize
    xmanager, 'mca_display::select_det', base, $
                event='mca_display_select_det_event'
    detector = *pdetector
end

;*****************************************************************************
pro mca_display_select_det_event, event
    ; called when select detector widget is modified

    widget_control, event.top, get_uvalue = top
    top.mca_display->select_det_event, event, top.detector, top.widgets
end

;*****************************************************************************
pro mca_display::select_det_event, event, detector, widgets

    widget_control, widgets.list, get_uvalue = pdetector
    *pdetector = event.index
    widget_control, event.top, /destroy
end

;*****************************************************************************
pro mca_display::open_file, file, background=background

    catch, ioerror
    if (ioerror ne 0) then begin
        t = dialog_message( /error, dialog = self.widgets.base, $
            ['Error reading file: ' + file + '.', $
             !err_string])
        if (keyword_set(background)) then begin
            self.background.valid=0
            self.background.is_detector=0
        endif else begin
            self.foreground.valid=0
            self.foreground.is_detector=0
        endelse
        self->update_spectrum, /rescale
        self->show_stats
        self->new_inputs
       return
    endif

    if (keyword_set(background)) then begin
        obj_destroy, self.background.mca
    endif else begin
        obj_destroy, self.foreground.mca
    endelse
    med = obj_new('med')
    med->read_file, file
    mcas = med->get_mca_objects()
    n_det = n_elements(mcas)
    if (n_det eq 1) then begin
        mca = mcas[0]
    endif else begin ; Multi-element detector
        detector = 0
        self->select_det, n_det, detector
        if (detector lt n_det) then begin
            mca = mcas[detector]
        endif else begin
            mca = mcas[0]
            data = med->get_data(/total, /align)
            mca->set_data, data
        endelse
    endelse
    obj_destroy, med
    self->open, mca, file, background=background

end


;*****************************************************************************
pro mca_display::open, mca, name, background=background

    ; Called when a new file or detector is opened
    if (obj_isa(mca, 'mca') ne 1) then return

    if (n_elements(name) eq 0) then name = ' '

    if (obj_isa(mca, 'hardware_mca') eq 1) then begin
        self.file.mca_name = name
        is_detector=1
    endif else begin
        self.file.filename = name
        is_detector=0
    endelse

    if (keyword_set(background)) then begin
        self.background.name = name
        self.background.valid = 1
        self.background.is_detector = is_detector
        self.background.mca = mca
        self.background.nchans = self.background.mca->get_nchans()
        self.background.roi = self.background.mca->get_rois(roi_info)
        self.background.nrois = roi_info.nrois
        self.background.elapsed = self.background.mca->get_elapsed()
        self.background.data = self.background.mca->get_data()
    endif else begin
        self.foreground.name = name
        self.foreground.valid = 1
        self.foreground.is_detector = is_detector
        self.foreground.mca = mca
        self.foreground.nchans = self.foreground.mca->get_nchans()
        self.foreground.roi = self.foreground.mca->get_rois(roi_info)
        self.foreground.nrois = roi_info.nrois
        self.foreground.elapsed = self.foreground.mca->get_elapsed()
        self.foreground.data = self.foreground.mca->get_data()
    endelse
    self->update_spectrum, /rescale
    self->show_stats
    self->new_inputs
end


;*****************************************************************************
pro mca_display::save_file, file
    openr, lun, /get, file, error=error
    if (error eq 0) then free_lun, lun
    if ((error eq 0) and self.options.warn_overwrite) then begin
        reply = dialog_message( /question, dialog = self.widgets.base, $
            'Warning - file: ' + file + ' already exists.  Overwrite file?')
        if (reply eq 'No') then return
    endif

    catch, ioerror
    if (ioerror ne 0) then begin
        t = dialog_message( /error, dialog = self.widgets.base,            $
            ['Unable to open file: ' + file, !err_string])
        return
    endif
    self.foreground.mca->write_file, file

    self.options.save_done = 1

    if (self.options.inform_save) then popup_program_info, time=2, $
        font=self.fonts.big_label, $
        title='MCA Display File Save',  $
        ['Successfully saved as file: ' + file]

    self.file.next_filename = increment_filename(file)
    widget_control, self.widgets.save_next, $
            set_value='Save Next = ' + self.file.next_filename

end


;*****************************************************************************
pro mca_display::print

    geometry=widget_info(self.widgets.base, /geometry)
    x = geometry.xoffset + geometry.xsize/2
    y = geometry.yoffset + geometry.ysize/2
    base = widget_base(/column, /frame, xoffset=x, yoffset=y)
    row = widget_base(base, /row, /frame)
    xsize = cw_field(row, /float, value=self.print.xsize, /column, $
                    title='X size (inches)', xsize=12)
    ysize = cw_field(row, /float, value=self.print.ysize, /column, $
                    title='Y size (inches)', xsize=12)
    orientation = cw_bgroup(row, ['Portrait', 'Landscape'], /column, $
                  label_top='Orientation', /exclusive, $
                  set_value=self.print.orientation)
    row = widget_base(base, /row, /frame)
    charsize = cw_field(row, /float, value=self.print.charsize, /column, $
                    title='Character size', xsize=12)
    thick = cw_field(row, /float, value=self.print.thick, /column, $
                    title='Line thickness', xsize=12)
    charthick = cw_field(row, /float, value=self.print.charthick, /column, $
                    title='Character thickness', xsize=12)
    font = cw_bgroup(row, ['Hardware', 'Vector'], /column, $
                  label_top='Font', /exclusive, $
                  set_value=self.print.font)
    row = widget_base(base, /row, /frame)
    xtitle = cw_field(row, /string, value=self.print.xtitle, /row, $
                    title='X title:', xsize=30)
    row = widget_base(base, /row, /frame)
    ytitle = cw_field(row, /string, value=self.print.ytitle, /row, $
                    title='Y title:', xsize=30)
    row = widget_base(base, /row, /frame)
    title = cw_field(row, /string, value=self.print.title, /row, $
                    title='Plot title:', xsize=30)
    row = widget_base(base, /row, /frame)
    row = widget_base(base, /row, /frame)
    print = widget_button(row, value='Print')
    quit = widget_button(row, value='Quit')

    widgets = {xsize: xsize, $
               ysize: ysize, $
               orientation: orientation, $
               charsize: charsize, $
               thick: thick, $
               charthick: charthick, $
               font: font, $
               xtitle: xtitle, $
               ytitle: ytitle, $
               title: title, $
               print: print, $
               quit: quit}
    widget_control, base, set_uvalue = {mca_display: self, widgets: widgets}
    widget_control, base, /realize
    xmanager, 'mca_display::print', base, $
                event='mca_display_print_event', /no_block

end

;*****************************************************************************
pro mca_display_print_event, event

    widget_control, event.top, get_uvalue = top
    top.mca_display->print_event, event, top.widgets
end

;*****************************************************************************
pro mca_display::print_event, event, widgets

if (event.id eq widgets.quit) then begin
    widget_control, event.top, /destroy
    return
endif

if (event.id eq widgets.print) then begin
    widget_control, widgets.xsize, get_value=xsize
    widget_control, widgets.ysize, get_value=ysize
    widget_control, widgets.orientation, get_value=orientation
    if (orientation eq 1) then landscape=1 else landscape=0
    widget_control, widgets.charsize, get_value=charsize
    widget_control, widgets.thick, get_value=thick
    widget_control, widgets.charthick, get_value=charthick
    widget_control, widgets.font, get_value=font
    if (font eq 1) then font_spec=-1 else font_spec=0
    widget_control, widgets.xtitle, get_value=xtitle
    xtitle = xtitle[0]
    widget_control, widgets.ytitle, get_value=ytitle
    ytitle = ytitle[0]
    widget_control, widgets.title, get_value=title
    title = title[0]
    lo = self.display.hmin
    hi = self.display.hmax
    nchans = hi-lo+1
    chans = lo + findgen(nchans)
    energy = self.foreground.mca->chan_to_energy(chans)
    calibration = self.foreground.mca->get_calibration()
    old_device = !d.name
    set_plot, 'printer'
    device, xsize=xsize, ysize=ysize, landscape=landscape, /inch
    plot, energy, self.foreground.data[lo:hi] > self.display.vmin,     $
          ylog = self.display.vlog,                              $
          yrange = [self.display.vmin, self.display.vmax],       $
          xstyle = 1,                                            $
          ystyle = 1,                                            $
          psym = self.display.psym,                              $
          charsize  = charsize,                                  $
          thick     = thick,                                     $
          charthick = charthick,                                 $
          xtitle    = xtitle,                                    $
          ytitle    = ytitle,                                    $
          title     = title,                                     $
          font      = font_spec
    if (self.background.valid) then $
        oplot, energy, self.background.data[lo:hi] > self.display.vmin, $
            psym   = self.display.psym,                              $
            thick  = thick

    x = .2
    y = .9
    chsize = charsize*.75
    ; Use filename if the foreground spectrum is a file or if it is a detector but has
    ; been saved to a file.
    name = self.file.filename
    if (self.foreground.valid and $
        self.foreground.is_detector and $
        (not self.options.save_done)) then name = self.foreground.name
    xyouts, x, y, /norm, size=chsize, charthick=thick, $
        'File: ' + name + '!C' + $
        'Date: ' + self.foreground.elapsed.start_time + '!C' + $
        'Live time: ' + $
        string(self.foreground.elapsed.live_time, format='(f10.2)') + '!C' + $
        'Real time: ' + $
        string(self.foreground.elapsed.real_time, format='(f10.2)') + '!C' + $
        'Calibration offset: ' + $
                string(calibration.offset, format='(f10.5)')+'!C'+$
        'Calibration slope: ' + $
                string(calibration.slope, format='(f10.5)')+'!C'+$
        'Calibration quad: ' + string(calibration.quad, format='(f10.5)')+'!C'+$
        '2-theta: '+ string(calibration.two_theta, format='(f10.4)')
    device, /close
    set_plot, old_device
    self.print.xsize = xsize
    self.print.ysize = ysize
    self.print.orientation = orientation
    self.print.charsize = charsize
    self.print.thick = thick
    self.print.charthick = charthick
    self.print.font = font
    self.print.xtitle = xtitle
    self.print.ytitle = ytitle
    self.print.title = title
    widget_control, event.top, /destroy
endif

end


;*****************************************************************************
pro mca_display::control_presets

    if xregistered( 'mca_control_presets') then return ; only one instance

    base = widget_base( title = 'MCA Presets', $
                        group = self.widgets.base, uvalue = self)

    presets = self.foreground.mca->get_presets()
    calibration = self.foreground.mca->get_calibration()
    form = cw_form( base, /column, [ $
        '1, BASE,   ,ROW, FRAME',                                    $
        '1, BASE,   ,COLUMN',                                        $
        '0, FLOAT,  '+ string( presets.real_time, format = '(f9.3)')+$
                    ',TAG =preal, LABEL_TOP =Real time,' +           $
                    'WIDTH =12',                                     $
        '0, FLOAT,  '+ string( presets.live_time, format = '(f9.3)') + $
                    ',TAG =plive, LABEL_TOP =Live time,' +           $
                    'WIDTH =12',                                     $
        '0, INTEGER,'+ string( presets.total_counts, format = '(i9)') +  $
                    ',TAG =ptotal, LABEL_TOP =Counts,' +             $
                    'WIDTH =12',                                     $
        '0, INTEGER,'+ string( presets.start_channel, format = '(i9)') +  $
                    ',TAG =pstart, LABEL_TOP = Start channel,' +     $
                    'WIDTH =12',                                     $
        '0, INTEGER, '+ string( presets.end_channel, format = '(i9)') +    $
                    ',TAG =pend, LABEL_TOP =End channel,' +          $
                    'WIDTH =12',                                     $
        '1, BASE,    ,ROW, FRAME',                                    $
        '0, BUTTON,  OK, QUIT, TAG =ok,' +                            $
                    'FONT ='+ self.fonts.big_button,                $
        '0, BUTTON,  Apply, QUIT, TAG =apply',                        $
        '2, BUTTON,  Cancel, QUIT, TAG =cancel'])

    widget_control, base, /realize
    xmanager, 'mca_display::control_presets', $
            event='mca_display_control_presets_event', base, /no_block

end


;*****************************************************************************
pro mca_display_display_jcpds_event, event
    widget_control, event.top, get_uvalue = top
    top.mca_display->display_jcpds_event, event, top.widgets
end
;*****************************************************************************
pro mca_display::display_jcpds_event, event, widgets


    case event.id of

        widgets.file: begin
            file = dialog_pickfile(/read, /must_exist, $
                                   path=self.file.jcpds_path, $
                                   get_path=path, filter='*.jcpds')
            if (file eq "") then return
            obj_destroy, self.file.jcpds
            self.file.jcpds=obj_new('JCPDS')
            self.file.jcpds_path = path
            start = strlen(path)
            short = strmid(file, start, 255)
            pos = strpos(short, '.')
            short = strmid(short, 0, pos)
            widget_control, widgets.label, set_value=short
            self.file.jcpds->read_file, file
        end

        widgets.pressure: begin
            widget_control, event.id, get_value=pressure
            self.display.pressure = pressure
        end

        widgets.temperature: begin
            widget_control, event.id, get_value=temperature
            self.display.temperature = temperature
        end

        widgets.two_theta: begin
            widget_control, event.id, get_value=two_theta
            calibration = self.background.mca->get_calibration()
            calibration.two_theta = two_theta
            self.background.mca->set_calibration, calibration
        end

        widgets.define_rois: begin
            r = self.file.jcpds->get_reflections()
            if (size(r, /type) eq 8) then begin
                npeaks = n_elements(r) < n_elements(self.foreground.roi)
            endif else begin
                npeaks = 0
            endelse
            for i=0, npeaks-1 do begin
                chan = self.background.mca->d_to_chan(r[i].d)
                if (chan gt 20 and chan lt self.foreground.nchans-21) then begin
                    left = chan-20
                    right = chan+20
                    widget_control, widgets.label, get_value=label
                    label = label + ' ' + $
                            strtrim(r[i].h,2) + $
                            strtrim(r[i].k,2) + $
                            strtrim(r[i].l,2)
                    roi = {MCA_ROI}
                    roi.left = left
                    roi.right = right
                    roi.label = label
                    roi.d_spacing = r[i].d
                    roi.use = 1
                    t = self.foreground.mca->add_roi(roi)
                endif
            endfor
            self.foreground.roi = self.foreground.mca->get_rois(roi_info)
            self.foreground.nrois = roi_info.nrois
            self->show_roi, 0
        end

        widgets.add_peaks: begin
            r = self.file.jcpds->get_reflections()
            if (size(r, /type) ne 8) then return
            if (not ptr_valid(self.fit.ppeaks)) then return
            peaks = *self.fit.ppeaks
            n = n_elements(r)
            for i=0, n-1 do begin
                new_peak = {mca_peak}
                chan = self.foreground.mca->d_to_chan(r[i].d)
                energy = self.foreground.mca->chan_to_energy(chan)
                new_peak.initial_energy = energy
                widget_control, widgets.label, get_value=label
                label = label + ' ' + $
                            strtrim(r[i].h,2) + $
                            strtrim(r[i].k,2) + $
                            strtrim(r[i].l,2)
                new_peak.label = label
                ; Make a reasonble estimate of initial FWHM
                new_peak.initial_fwhm = 0.2 + .03*sqrt(abs(energy))
                new_peak.energy_flag=1
                new_peak.fwhm_flag=1
                peaks = [peaks, new_peak]
            endfor
            t = dialog_message(/inform, 'Added ' + strtrim(n, 2) + $
                               ' peaks to the peak list')
            self.fit.ppeaks = ptr_new(peaks)
        end

        widgets.exit: begin
            widget_control, event.top, /destroy
            return
        end
    endcase

    self->draw_jcpds, self.file.jcpds

end


;*****************************************************************************
pro mca_display::draw_jcpds, in_jcpds

    if (not obj_valid(self.background.mca)) then begin
        self.background.mca = obj_new('mca')
        self.background.nchans = self.foreground.nchans
        calibration = self.foreground.mca->get_calibration()
        self.background.mca->set_calibration, calibration
        self.background.valid=1
        self.background.is_detector=0
    endif

    jcpds = in_jcpds
    catch, error
    if (error) then begin
        t = dialog_message(!error_state.msg)
        goto, skip
    endif
    jcpds->compute_d, self.display.pressure, self.display.temperature

skip:
    data = lonarr(self.background.nchans)
    r = jcpds->get_reflections()
    if (size(r, /type) ne 8) then return
    chan = self.background.mca->d_to_chan(r.d)
    data[chan] = r.inten
    fmax = max(self.foreground.data)
    data = data * (fmax / 100.)
    self.background.mca->set_data, data
    self->update_spectrum, /rescale
end

;*****************************************************************************
pro mca_display::display_jcpds

    if xregistered( 'mca_display::display_jcpds') then return

    obj_destroy, self.background.mca
    calibration = self.foreground.mca->get_calibration()

    widgets = { $
        base: 0L, $
        label: 0L, $
        file: 0L, $
        pressure: 0L, $
        temperature: 0L, $
        define_rois: 0L, $
        add_peaks: 0L, $
        two_theta: 0L, $
        exit: 0L  $
    }

    base = widget_base( title = 'MCA Display JCPDS', $
                        group = self.widgets.base, /column)

    widgets.base = base
    row = widget_base(base, /row)
    widgets.file = widget_button(row, value='Material')
    widgets.label = widget_label(row, value='                    ', /frame)
    widgets.pressure = cw_field(base, /float, title='Pressure (GPa):', $
                                value=0., /return_events)
    widgets.temperature = cw_field(base, /float, title='Temperature (K):', $
                                value=0., /return_events)
    widgets.two_theta = cw_field(base, /float, $
                                title='2-theta (degrees):', $
                                value=calibration.two_theta, /return_events)
    row = widget_base(base, /row)
    widgets.define_rois = widget_button(row, value='Add ROIS')
    widgets.add_peaks = widget_button(row, value='Add Peaks')
    widgets.exit = widget_button(row, value='Exit')

    widget_control, base, /realize
    widget_control, base, set_uvalue={mca_display: self, widgets: widgets}
    xmanager, 'mca_display::display_jcpds', $
            event='mca_display_display_jcpds_event', base, /no_block
end


;*****************************************************************************
pro mca_display::update_peak_fit_widgets, widgets, peaks, current
    widget_control, widgets.exponent, get_uvalue=values
    index = where(self.fit.background.exponent eq values)
    widget_control, widgets.exponent, set_droplist_select=index[0]
    widget_control, widgets.top_width, $
                    set_value=self.fit.background.top_width
    widget_control, widgets.bottom_width, $
                    set_value=self.fit.background.bottom_width
    widget_control, widgets.tangent, $
                    set_value=self.fit.background.tangent
    widget_control, widgets.compress, get_uvalue=values
    index = where(self.fit.background.compress eq values)
    widget_control, widgets.compress, set_droplist_select=index[0]
    widget_control, widgets.energy_cal_offset, $
                    set_value=self.fit.fit.initial_energy_offset
    widget_control, widgets.energy_cal_slope, $
                    set_value=self.fit.fit.initial_energy_slope
    widget_control, widgets.energy_cal_flag, $
                    set_droplist_select=self.fit.fit.energy_flag
    widget_control, widgets.fwhm_cal_offset, $
                    set_value=self.fit.fit.initial_fwhm_offset
    widget_control, widgets.fwhm_cal_slope, $
                    set_value=self.fit.fit.initial_fwhm_slope
    widget_control, widgets.fwhm_cal_flag, $
                    set_droplist_select=self.fit.fit.fwhm_flag
    text = ''
    for i=0, n_elements(peaks)-1 do begin
        str = string(peaks[i].label, format='(a15)') + ', ' + $
              string(peaks[i].initial_energy, format='(f10.4)') + ', ' + $
              string(peaks[i].energy_flag, format='(i2)') + ', ' + $
              string(peaks[i].initial_fwhm, format='(f10.4)') + ', ' + $
              string(peaks[i].fwhm_flag, format='(i2)') + ', ' + $
              string(peaks[i].ampl_factor, format='(f10.4)')
        if (i eq 0) then text = str else text = [text, str]
    endfor
    widget_control, widgets.peak_list, set_value=text
    index = current > 0 < (n_elements(peaks)-1)
    widget_control, widgets.peak_list, set_list_select=index
    if (n_elements(peaks) gt 0) then $
        self->update_peak_line, widgets, peaks[index]
end


;*****************************************************************************
pro mca_display::update_peak_line, widgets, peak
    widget_control, widgets.label, set_value=peak.label
    widget_control, widgets.energy, set_value=peak.initial_energy
    widget_control, widgets.energy_flag, $
                    set_droplist_select=peak.energy_flag
    widget_control, widgets.fwhm, set_value=peak.initial_fwhm
    widget_control, widgets.fwhm_flag, $
                    set_droplist_select=peak.fwhm_flag
    widget_control, widgets.ampl_factor, set_value=peak.ampl_factor
end


;*****************************************************************************
pro mca_display::fit_background, widgets
    widget_control, /hourglass
    self.background.mca = self.foreground.mca->fit_background( $
                              exponent=self.fit.background.exponent, $
                              compress=self.fit.background.compress, $
                              tangent=self.fit.background.tangent, $
                              top_width=self.fit.background.top_width, $
                              bottom_width=self.fit.background.bottom_width)
    self.background.valid=1
    self.background.is_detector=0
    self->update_spectrum, /rescale
    self.background.name = 'Background fit'
    self->new_inputs ; Update title bar
    ptr_free, self.fit.pbackground
    self.fit.pbackground = ptr_new(self.background.mca->get_data())
end

pro mca_display::mark_peak_positions, peaks
    obj_destroy, self.background.mca
    self.background.mca = self.foreground.mca->copy()
    chans = self.background.mca->energy_to_chan(peaks.initial_energy) > 0
    data = self.background.mca->get_data()
    temp = data*0
    temp[chans] = data[chans]
    self.background.mca->set_data, temp
    self.background.valid=1
    self.background.is_detector=0
    self->update_spectrum, /rescale
    self.background.name = 'Peak positions'
    self->new_inputs  ; Update title bar
end


;*****************************************************************************
pro mca_display_peak_fit_event, event
    widget_control, event.top, get_uvalue = uvalue
    uvalue.mca_display->peak_fit_event, event
end

;*****************************************************************************
pro mca_display::peak_fit_event, event
    widget_control, event.top, get_uvalue=uvalue
    widgets = uvalue.widgets
    if (ptr_valid(self.fit.ppeaks)) then peaks = *(self.fit.ppeaks)
    index = widget_info(widgets.peak_list, /list_select)
    if (index lt 0) then index=0

    case event.id of

        widgets.read_peak_file: begin
            file = dialog_pickfile(/read, /must_exist)
            if (file eq '') then goto, done
            ptr_free, self.fit.ppeaks
            peaks = read_peaks(file, background)
            self.fit.background = background
            index = 0
        end

        widgets.write_peak_file: begin
            file = dialog_pickfile(/write)
            if (file eq '') then goto, done
            write_peaks, peaks, file, self.fit.background
        end

        widgets.set_results_file: begin
            file = dialog_pickfile(/write)
            if (file ne '') then begin
                self.file.fit_results = file
                widget_control, widgets.results_file_name, set_value=file
            endif
        end

        widgets.set_spreadsheet_file: begin
            file = dialog_pickfile(/write)
            if (file ne '') then begin
                self.file.fit_spreadsheet = file
                widget_control, widgets.spreadsheet_file_name, set_value=file
            endif
        end

        widgets.exit: begin
            widget_control, event.top, /destroy
            return
        end

        widgets.exponent: begin
            widget_control, widgets.exponent, get_uvalue=values
            self.fit.background.exponent = values[event.index]
        end

        widgets.top_width: begin
            self.fit.background.top_width = event.value
        end

        widgets.bottom_width: begin
            self.fit.background.bottom_width = event.value
        end

        widgets.tangent: begin
            self.fit.background.tangent = event.value
        end

        widgets.compress: begin
            widget_control, widgets.compress, get_uvalue=values
            self.fit.background.compress = values[event.index]
        end

        widgets.fit_background: begin
            self->fit_background, widgets
        end

        widgets.energy_cal_offset: begin
            self.fit.fit.initial_energy_offset = event.value
        end

        widgets.energy_cal_slope: begin
            self.fit.fit.initial_energy_slope = event.value
        end

        widgets.energy_cal_flag: begin
            self.fit.fit.energy_flag = event.index
        end

        widgets.fwhm_cal_offset: begin
            self.fit.fit.initial_fwhm_offset = event.value
        end

        widgets.fwhm_cal_slope: begin
            self.fit.fit.initial_fwhm_slope = event.value
        end

        widgets.fwhm_cal_flag: begin
            self.fit.fit.fwhm_flag = event.index
        end

        widgets.label: begin
            label = strtrim(event.value[0],2)
            peaks[index].label = label
            ; See if this is an XRF or gamma line
            energy = lookup_xrf_line(label)
            if (energy eq 0.) then energy = lookup_gamma_line(label)
            if (energy gt 0.) then begin
                peaks[index].initial_energy=energy
                ; Set the energy flag to 0, since energy is known
                peaks[index].energy_flag=0
                ; Set FWHM flag to 0, since peak width will be defined by
                ;   detector resolution
                peaks[index].fwhm_flag=0
            endif
        end

        widgets.energy: begin
            peaks[index].initial_energy = event.value
        end

        widgets.energy_flag: begin
            peaks[index].energy_flag = event.index
        end

        widgets.fwhm: begin
            peaks[index].initial_fwhm = event.value
        end

        widgets.fwhm_flag: begin
            peaks[index].fwhm_flag = event.index
        end

        widgets.ampl_factor: begin
            peaks[index].ampl_factor = event.value
        end

        widgets.insert: begin
            new_peak = {mca_peak}
            new_peak.label = '    '
            energy = self.foreground.mca->chan_to_energy(self.display.cursor)
            new_peak.initial_energy = energy
            ; Make a reasonble estimate of initial FWHM
            new_peak.initial_fwhm = 0.2 + .03*sqrt(abs(energy))
            new_peak.energy_flag=1
            new_peak.fwhm_flag=1
            num=n_elements(peaks)
            if (index eq 0) then peaks = [new_peak, peaks] $
            else peaks = [peaks[0:index-1], new_peak, peaks[index:num-1]]
            self->mark_peak_positions, peaks
        end

        widgets.append: begin
            new_peak = {mca_peak}
            new_peak.label = '    '
            energy = self.foreground.mca->chan_to_energy(self.display.cursor)
            new_peak.initial_energy = energy
            ; Make a reasonble estimate of initial FWHM
            new_peak.initial_fwhm = 0.2 + .03*sqrt(abs(energy))
            new_peak.energy_flag=1
            new_peak.fwhm_flag=1
            num=n_elements(peaks)
            if (index eq num-1) then peaks = [peaks, new_peak] $
            else peaks = [peaks[0:index], new_peak, peaks[index+1:num-1]]
            index = index + 1
            self->mark_peak_positions, peaks
        end

        widgets.delete: begin
            num = widget_info(widgets.peak_list, /list_number)
            if ((index ge 0) and (num gt 0)) then begin
                if (num eq 1) then peaks = {mca_peak} else begin
                    good = indgen(num)
                    good = good(where(good ne index))
                    peaks = peaks[good]
                endelse
            endif
            self->mark_peak_positions, peaks
        end

        widgets.clear_peaks: begin
            response = dialog_message(/question, 'Clear all peaks?')
            if (response eq 'Yes') then begin
                peaks = {mca_peak}
                self->mark_peak_positions, peaks
            endif
        end

        widgets.sort: begin
            s = sort(peaks.initial_energy)
            peaks = peaks[s]
            self->mark_peak_positions, peaks
        end

        widgets.peak_list: begin
            peak = peaks[event.index]
            chan = self.foreground.mca->energy_to_chan(peak.initial_energy)
            self->cursor, chan
            self->lmarker, chan-1
            self->rmarker, chan+1
            self->mark_peak_positions, peaks
        end

        widgets.append_mode: begin
        end

        widgets.mark_peaks: begin
            self->mark_peak_positions, peaks
        end

        widgets.plot_background: begin
            self.background.mca->set_data, *self.fit.pbackground
            self.background.valid=1
            self.background.is_detector=0
            self->update_spectrum, /rescale
            self.background.name = 'Background fit'
            self->new_inputs ; Update title bar
        end

        widgets.plot_fit: begin
            self.background.mca->set_data, *self.fit.pfit
            self.background.valid=1
            self.background.is_detector=0
            self->update_spectrum, /rescale
            self.background.name = 'Peak fit'
            self->new_inputs  ; Update title bar
        end

        widgets.fit_peaks: begin
            widget_control, /hourglass
            self->fit_background, widgets
            background = self.background.mca
            append = widget_info(widgets.append_mode, /droplist_select)
            result = self.foreground.mca->fit_peaks($
                        peaks, fit=self.fit.fit, background=background, $
                        output=self.file.fit_results, $
                        spreadsheet=self.file.fit_spreadsheet, append=append)
            ptr_free, self.fit.pfit
            self.fit.pfit = ptr_new(result->get_data())
            obj_destroy, self.background.mca
            self.background.mca = result
            self.background.valid=1
            self.background.is_detector=0
            self->update_spectrum, /rescale
            self.background.name = 'Peak fit'
            self->new_inputs  ; Update title bar
            xdisplayfile, self.file.fit_results, width=85, $
                          font=get_font_name(/courier, /medium)
        end

        else: begin
            print, 'Unknown widget event'
        end
    endcase

    done:
    self->update_peak_fit_widgets, widgets, peaks, index
    self.fit.ppeaks = ptr_new(peaks)

end


;*****************************************************************************
pro mca_display::peak_fit

    if xregistered( 'mca_display::peak_fit') then return ; only one instance

    widgets = { $
        base: 0L, $
        exponent: 0L, $
        top_width: 0L, $
        bottom_width: 0L, $
        tangent: 0L, $
        compress: 0L, $
        label: 0L, $
        energy: 0L, $
        energy_flag: 0L, $
        fwhm: 0L, $
        fwhm_flag: 0L, $
        ampl_factor: 0L, $
        peak_list: 0L, $
        energy_cal_offset: 0L, $
        energy_cal_slope: 0L, $
        energy_cal_flag: 0L, $
        fwhm_cal_offset: 0L, $
        fwhm_cal_slope: 0L, $
        fwhm_cal_flag: 0L, $
        insert: 0L, $
        append: 0L, $
        delete: 0L, $
        clear_peaks: 0L, $
        sort: 0L, $
        fit_background: 0L, $
        mark_peaks: 0L, $
        plot_fit: 0L, $
        plot_background: 0L, $
        fit_peaks: 0L, $
        read_peak_file: 0L, $
        write_peak_file: 0L, $
        set_results_file: 0L, $
        set_spreadsheet_file: 0L, $
        results_file_name: 0L, $
        spreadsheet_file_name: 0L, $
        append_mode: 0L, $
        exit: 0L $
    }

    xsize=10
    base = widget_base(title='Peak Fit', mbar=mbar, /column, $
                       /base_align_center)
    widgets.base = base

    file = widget_button(mbar, /menu, value='File')
    widgets.read_peak_file = widget_button(file, value='Read peaks ...')
    widgets.write_peak_file = widget_button(file, value='Write peaks ...')
    widgets.set_results_file = widget_button(file, value='Results file ...')
    widgets.set_spreadsheet_file = widget_button(file, value='Spreadsheet file ...')
    widgets.exit = widget_button(file, value='Exit')

    bcol = widget_base(base, /column, /frame, /base_align_center)
    row = widget_base(bcol, /row, /align_center)
    t = widget_label(row, value='Background Parameters', $
                font=get_font_name(/large, /bold))
    row = widget_base(bcol, /row)
    col = widget_base(row, /column)
    t = widget_label(col, value='Exponent')
    choices = [2, 4, 6]
    widgets.exponent = widget_droplist(col, value=strtrim(choices,2), $
                                            uvalue=choices)
    widgets.top_width = cw_field(row, title='Top width', /column, $
                            /float, xsize=xsize, /return_events)
    widgets.bottom_width = cw_field(row, title='Bottom width', /column, $
                            /float, xsize=xsize, /return_events)
    widgets.tangent = cw_bgroup(row, ['No', 'Yes'], label_top='Tangent', $
                                /exclusive, /row, /no_release)
    col = widget_base(row, /column)
    t = widget_label(col, value='Compression')
    choices = [1, 2, 4, 8, 16]
    widgets.compress = widget_droplist(col, value=strtrim(choices,2), $
                                            uvalue=choices)

    row = widget_base(bcol, /row)
    widgets.fit_background = widget_button(row, value='Fit background')
    widgets.plot_background = widget_button(row, value='Re-plot background')

    fcol = widget_base(base, /column, /frame, /base_align_center)
    row = widget_base(fcol, /row, /align_center)
    t = widget_label(row, value='Peak Fit Parameters', $
                font=get_font_name(/large, /bold))
    row = widget_base(fcol, /row)
    t = widget_label(row, value='Initial energy calibration: ')
    widgets.energy_cal_offset = cw_field(row, title='Offset', /column, $
                                /return_events, /float, xsize=xsize)
    widgets.energy_cal_slope = cw_field(row, title='Slope', /column, $
                                /return_events, /float, xsize=xsize)

    col = widget_base(row, /column)
    t = widget_label(col, value='Flag')
    choices = ['Fix', 'Optimize']
    widgets.energy_cal_flag = widget_droplist(col, value=choices)
    row = widget_base(fcol, /row)
    t = widget_label(row, value='Initial FWHM calibration: ')
    widgets.fwhm_cal_offset = cw_field(row, title='Offset', /column, $
                                /return_events, /float, xsize=xsize)
    widgets.fwhm_cal_slope = cw_field(row, title='Slope', /column, $
                                /return_events, /float, xsize=xsize)

    col = widget_base(row, /column)
    t = widget_label(col, value='Flag')
    choices = ['Fix', 'Optimize']
    widgets.fwhm_cal_flag = widget_droplist(col, value=choices)

    row = widget_base(fcol, /row)
    widgets.label = cw_field(row, title='Label', /column, /return_events, $
                                xsize=xsize)
    widgets.energy = cw_field(row, title='Energy', /column, /return_events, $
                                /float, xsize=xsize)
    col = widget_base(row, /column)
    t = widget_label(col, value='Energy flag')
    choices = ['Fix', 'Optimize']
    widgets.energy_flag = widget_droplist(col, value=choices)
    widgets.fwhm = cw_field(row, title='FWHM', /column, /return_events, $
                                /float, xsize=xsize)
    col = widget_base(row, /column)
    t = widget_label(col, value='FWHM flag')
    choices = ['Global', 'Optimize', 'Fix']
    widgets.fwhm_flag = widget_droplist(col, value=choices)
    widgets.ampl_factor = cw_field(row, title='Ampl. ratio', /column, $
                                /return_events, /float, xsize=xsize)

    row = widget_base(fcol, /row)
    col = widget_base(row, /column)
    widgets.insert = widget_button(col, value='Insert')
    widgets.append = widget_button(col, value='Append')
    widgets.delete = widget_button(col, value='Delete')
    widgets.clear_peaks = widget_button(col, value='Clear')
    widgets.sort = widget_button(col, value='Sort')
    widgets.mark_peaks = widget_button(col, value='Mark all')
    text = ' '
    widgets.peak_list = widget_list(row, ysize=10, xsize=60, value=text, $
                                    font=get_font_name(/courier, /medium))

    row = widget_base(fcol, /row, /align_left)
    widgets.results_file_name = cw_field(row, value=self.file.fit_results, $
                                         title='Fit results file', $
                                         /row, /noedit, xsize=40)

    row = widget_base(fcol, /row, /align_left)
    widgets.spreadsheet_file_name = cw_field(row, value=self.file.fit_spreadsheet, $
                                         title='Fit spreadsheet file', $
                                         /row, /noedit, xsize=40)
    row = widget_base(fcol, /row, /align_left)
    t = widget_label(row, value='Overwrite or append results and spreadsheet files')
    widgets.append_mode = widget_droplist(row, value=['Overwrite', 'Append'])
    widget_control, widgets.append_mode, set_droplist_select=1

    row = widget_base(fcol, /row)
    widgets.fit_peaks = widget_button(row, value='Fit peaks')
    widgets.plot_fit = widget_button(row, value='Re-plot fit')

    geometry = widget_info(fcol, /geometry)
    widget_control, bcol, xsize=geometry.xsize

    fit = self.foreground.mca->fit_initialize()
    self.fit.fit.initial_energy_offset = fit.initial_energy_offset
    self.fit.fit.initial_energy_slope = fit.initial_energy_slope
    self.fit.fit.initial_fwhm_offset = fit.initial_fwhm_offset
    self.fit.fit.initial_fwhm_slope = fit.initial_fwhm_slope

    if (ptr_valid(self.fit.ppeaks)) then peaks = *(self.fit.ppeaks)
    self->update_peak_fit_widgets, widgets, peaks, 0

    uvalue = {mca_display: self, widgets: widgets}
    widget_control, base, /realize, set_uvalue=uvalue
    xmanager, "mca_display::peak_fit", base, group_leader=self.widgets.base, $
        event="mca_display_peak_fit_event", /no_block
end


;*****************************************************************************
pro mca_display_control_presets_event, event
    widget_control, event.top, get_uvalue = mca_display
    mca_display->control_presets_event, event

end
;*****************************************************************************
pro mca_display::control_presets_event, event

    if not event.quit then return ; wait for OK, Apply or Cancel

    widget_control, event.id, get_value = form
    presets = self.foreground.mca->get_presets()
    calibration = self.foreground.mca->get_calibration()
    if form.ok or form.apply then begin
        presets.real_time  = form.preal
        presets.live_time  = form.plive
        presets.total_counts = form.ptotal
        presets.start_channel = form.pstart
        presets.end_channel   = form.pend
        self.foreground.mca->set_presets, presets
    endif

    if (form.ok or form.cancel) then widget_control, event.top, /destroy
end


;*****************************************************************************
pro mca_display_cal_energy_event, event

    widget_control, event.top, get_uvalue = cal, /no_copy
    cal.mca_display->cal_energy_event, event, cal
end

;*****************************************************************************
pro mca_display::cal_energy_event, event, cal

    nrois = cal.nrois
    calibration = cal.mca->get_calibration()

    case event.id of

        cal.widgets.do_fit: begin
            degree = widget_info(cal.widgets.fit_type, /droplist_select)
            degree = degree + 1
            ; Find which ROIs should be used for the calibration
            use = where ((cal.roi[0:nrois-1].use eq 1), nuse)
            if ((degree eq 1) and (nuse lt 2)) then begin
                t = dialog_message( /error, dialog=cal.widgets.base, $
                  'Must have at least two valid points for linear calibration')
                goto, done
            endif else if ((degree eq 2) and (nuse lt 3)) then begin
                t = dialog_message( /error, dialog=cal.widgets.base, $
                  'Must have at least three valid points for '+ $
                        'quadratic calibration')
                goto, done
            endif
            chan=fltarr(nuse) & energy=fltarr(nuse)
            for i=0, nuse-1 do begin
                chan[i]   = cal.roi(use[i]).centroid
                energy[i] = cal.roi(use[i]).energy
            endfor
            coeffs = poly_fit(chan, energy, degree, e_pred)

            calibration.offset = coeffs[0]
            widget_control, cal.widgets.cal_offset, $
                set_value=string(calibration.offset, format='(g12.5)')
            calibration.slope = coeffs[1]
            widget_control, cal.widgets.cal_slope, $
                set_value=string(calibration.slope, format='(g12.5)')
            if (degree eq 2) then calibration.quad = coeffs[2] else $
                calibration.quad = 0.0
            cal.mca->set_calibration, calibration
            widget_control, cal.widgets.cal_quad, $
                set_value=string(calibration.quad, format='(g12.5)')
            for i=0, nrois-1 do begin
                energy_diff = cal.roi[i].energy - $
                    cal.mca->chan_to_energy(cal.roi[i].centroid)
                widget_control, cal.widgets.energy_diff[i], $
                    set_value=string(energy_diff, format='(f10.4)')
                ; Recompute FWHM
                cal.fwhm[i] = cal.mca->chan_to_energy(cal.roi[i].centroid + $
                                                  cal.fwhm_chan[i]/2.) - $
                              cal.mca->chan_to_energy(cal.roi[i].centroid - $
                                                  cal.fwhm_chan[i]/2.)
                widget_control, cal.widgets.fwhm[i], $
                    set_value=string(cal.fwhm[i], format='(f10.3)')
            endfor
        end

        cal.widgets.plot_cal: begin
            use = where ((cal.roi[0:nrois-1].use eq 1), nuse)
            if (nuse le 0) then goto, done
            energy = fltarr(nrois) & energy_diff = fltarr(nrois)
            for i=0, nrois-1 do begin
                energy[i] = cal.roi[i].energy
                energy_diff[i] = cal.roi[i].energy - $
                                 cal.mca->chan_to_energy(cal.roi[i].centroid)
            endfor
            window, self.windows.energy_cal, title='MCA Calibration Error'
            plot, energy, energy_diff, psym=1, charsize=1.5, $
                xtitle='Energy', ytitle='Calibration error'
            oplot, energy[use], energy_diff[use]
        end

        cal.widgets.plot_fwhm: begin
            energy = fltarr(nrois) & fwhm = fltarr(nrois)
            for i=0, nrois-1 do begin
                energy[i] = cal.roi[i].energy
                fwhm[i]   = cal.fwhm[i]
            endfor
            window, self.windows.energy_cal, title='MCA FWHM'
            plot, energy, fwhm, psym=-1,  charsize=1.5, $
                xtitle='Energy', ytitle='FWHM'
        end

        cal.widgets.cal_units: begin
            calibration.units = event.value
            cal.mca->set_calibration, calibration
        end


        cal.widgets.OK: begin
            self.foreground.mca->set_calibration, calibration
            self.foreground.mca->set_rois, cal.roi(0:nrois-1)
            self->update_spectrum, /rescale
            ; Update calibration text fields
            self->lmarker, self.display.lmarker
            self->rmarker, self.display.rmarker
            self->cursor,  self.display.cursor
            self->show_stats
                widget_control, event.top, /destroy
            obj_destroy, cal.mca
            return
        end

        cal.widgets.cancel: begin
            widget_control, event.top, /destroy
            obj_destroy, cal.mca
            return
        end

        else: begin
            for i=0, nrois-1 do begin
                if (event.id eq cal.widgets.line[i]) then begin
                    widget_control, event.id, get_value=str
                    energy = lookup_xrf_line(str[0])
                    if (energy eq 0.) then energy = lookup_gamma_line(str[0])
                    cal.roi[i].energy = energy
                    widget_control, cal.widgets.energy[i], $
                        set_value=string(energy)
                endif else if (event.id eq cal.widgets.energy[i]) then begin
                    widget_control, event.id, get_value=str
                    energy = float(str[0])
                    cal.roi[i].energy = energy
                    widget_control, cal.widgets.energy[i], $
                        set_value=string(energy)
                endif else if (event.id eq cal.widgets.centroid[i]) then begin
                    widget_control, event.id, get_value=str
                    centroid = float(str[0])
                    cal.roi[i].centroid = centroid
                    widget_control, cal.widgets.centroid[i], $
                        set_value=string(centroid)
                endif else if (event.id eq cal.widgets.use_flag[i]) then begin
                    cal.roi[i].use = widget_info(event.id, /droplist_select)
                endif
            endfor
        end
    endcase
    done:
    widget_control, event.top, set_uvalue = cal, /no_copy
end


;**********************************************************************
pro mca_display::cal_energy

    nrois = self.foreground.nrois
    if (nrois lt 2) then begin
        t = dialog_message( /error, $
                'Must have at least two ROIs to perform calibration')
        return
    endif

    widgets = { $
        base:           0L, $
        use_flag:       lonarr(nrois), $
        centroid:       lonarr(nrois), $
        fwhm:           lonarr(nrois), $
        energy:         lonarr(nrois), $
        line:           lonarr(nrois), $
        energy_diff:    lonarr(nrois), $
        cal_units:      0L, $
        cal_offset:     0L, $
        cal_slope:      0L, $
        cal_quad:       0L, $
        fit_type:       0L, $
        do_fit:         0L, $
        plot_cal:       0L, $
        plot_fwhm:      0L, $
        cancel:         0L, $
        OK:             0L  $
    }

    calibration = self.foreground.mca->get_calibration()
    roi = self.foreground.mca->get_rois()
    cal = {                             $
        mca:            obj_new('mca'),  $
        mca_display:    self,            $
        fwhm:           fltarr(nrois),   $
        fwhm_chan:      fltarr(nrois),   $
        nrois:          nrois,           $
        roi:            roi,             $
        widgets:        widgets          $
    }
    cal.mca->set_calibration, calibration

    ; Compute the centroid and FWHM of each ROI
    for i=0, nrois-1 do begin
        left = cal.roi[i].left
        right = cal.roi[i].right
        total_counts = self.foreground.data[left:right]
        n_sel        = right - left + 1L
        sel_chans    = left + indgen(n_sel)
        left_counts  = self.foreground.data[left]
        right_counts = self.foreground.data[right]
        bgd_counts   = left_counts + findgen(n_sel)/(n_sel-1) * $
                                 (right_counts - left_counts)
        net_counts   = total_counts - bgd_counts
        net          = total(net_counts)

        if ((net gt 0.) and (n_sel ge 3)) then begin
            mca_display_fit_gaussian, sel_chans, net_counts, amplitude, $
                                  centroid, fwhm
            cal.roi[i].centroid = centroid
            cal.fwhm_chan[i] = fwhm
        endif else begin
            cal.roi[i].centroid = (left + right)/2.
            cal.fwhm[i] = right-left
        endelse
        cal.fwhm[i] = cal.mca->chan_to_energy(cal.roi[i].centroid + $
                                          cal.fwhm_chan[i]/2.) - $
                      cal.mca->chan_to_energy(cal.roi[i].centroid - $
                                          cal.fwhm_chan[i]/2.)
    endfor

    base = widget_base(title="MCA Calibrate Energy", /column)
    cal.widgets.base = base

    row = widget_base(base, /row, /frame)
    ; Determine height for all of the widgets in this row
    dummy = widget_text(base, xsize=6)
    geometry = widget_info(dummy, /geometry)
    widget_control, dummy, /destroy
    scr_ysize = geometry.scr_ysize
    scr_xsize = geometry.scr_xsize

    col = widget_base(row, /column)
    t = widget_label(col, value='ROI', font=self.fonts.label)
    for i=0, nrois-1 do begin
        t = widget_label(col, value=strtrim(string(i),2), scr_ysize=scr_ysize)
    endfor

    col = widget_base(row, /column)
    t = widget_label(col, value='Use?', font=self.fonts.label)
    for i=0, nrois-1 do begin
        cal.widgets.use_flag[i] = $
            widget_droplist(col, value=['No','Yes'], scr_ysize=scr_ysize)
        widget_control, cal.widgets.use_flag[i], $
                    set_droplist_select=cal.roi[i].use
    endfor

    col = widget_base(row, /column)
    t = widget_label(col, value='Centroid', font=self.fonts.label)
    for i=0, nrois-1 do begin
        cal.widgets.centroid[i] = $
;            widget_text(col, /edit, value=string(cal.roi[i].centroid), $
;                        scr_ysize=scr_ysize, xsize=10)
;  The scr_ysize in the calls to widget_text here and below is not needed
;  (it will be the default) and does not work on Windows NT in IDL 5.0
            widget_text(col, /edit, value=string(cal.roi[i].centroid), $
                        xsize=10)
    endfor

    col = widget_base(row, /column)
    t = widget_label(col, value='FWHM', font=self.fonts.label)
    for i=0, nrois-1 do begin
        cal.widgets.fwhm[i] = $
            widget_text(col, value=string(cal.fwhm[i]), $
                        xsize=10)
    endfor

    col = widget_base(row, /column)
    t = widget_label(col, value='Energy', font=self.fonts.label)
    for i=0, nrois-1 do begin
        ; If the ROI energy is zero, then try to use the label to lookup an
        ; XRF line energy
        if (cal.roi[i].energy eq 0.0) then $
            cal.roi[i].energy = lookup_xrf_line(cal.roi[i].label)
            if (cal.roi[i].energy eq 0.) then $
                cal.roi[i].energy = lookup_gamma_line(cal.roi[i].label)
        cal.widgets.energy[i] = $
            widget_text(col, /edit, value=string(cal.roi[i].energy), $
                        xsize=10)
    endfor

    col = widget_base(row, /column)
    t = widget_label(col, value='Fluor. line', font=self.fonts.label)
    for i=0, nrois-1 do begin
        cal.widgets.line[i] = $
            widget_text(col, /edit, value=cal.roi[i].label, $
                        xsize=8)
    endfor

    col = widget_base(row, /column)
    t = widget_label(col, value='Energy diff.', font=self.fonts.label)
    for i=0, nrois-1 do begin
        cal.widgets.energy_diff[i] = $
            widget_text(col, value=string(0.0), $
                        xsize=10)
    endfor

    row = widget_base(base, /row, /frame)
    r = widget_base(row, /row)
    t = widget_label(r, value='Calibration type:', font=self.fonts.label)
    cal.widgets.fit_type  = widget_droplist(r, value=['Linear', 'Quadratic'])
    cal.widgets.do_fit    = widget_button(row, value='Compute calibration')
    cal.widgets.plot_cal  = widget_button(row, value='Plot calibration error')
    cal.widgets.plot_fwhm = widget_button(row, value='Plot FWHM')

    row = widget_base(base, /row, /frame)
    t = widget_label(row, value='Calibration coefficients:', $
                font=self.fonts.label)
    xsize=12
    cal.widgets.cal_units  = cw_field(row, title='Units',                $
                                  value=calibration.units,         $
                                  /column, /string, xsize=xsize,     $
                                  /return_events)
    cal.widgets.cal_offset = cw_field(row, title='Offset',               $
                                  value=calibration.offset,        $
                                  /column, /float, /noedit, xsize=xsize)
    cal.widgets.cal_slope  = cw_field(row, title='Slope',                $
                                  value=calibration.slope,         $
                                  /column, /float, /noedit, xsize=xsize)
    cal.widgets.cal_quad   = cw_field(row, title='Quadratic',            $
                                  value=calibration.quad,          $
                                  /column, /float, /noedit, xsize=xsize)

    row = widget_base(base, /row, /frame)
    cal.widgets.OK = widget_button(row, value="OK", $
                 font=self.fonts.big_button)
    cal.widgets.cancel = widget_button(row, value="Cancel", $
                 font=self.fonts.big_button)
    widget_control, base, set_uvalue=cal
    widget_control, base, /realize
    xmanager, "mca_display::cal_energy", base, group_leader=self.widgets.base, $
        event="mca_display_cal_energy_event", /no_block
end


;*****************************************************************************
pro mca_display_cal_two_theta_event, event

    widget_control, event.top, get_uvalue = cal, /no_copy
    cal.mca_display->cal_two_theta_event, event, cal
end

;*****************************************************************************
pro mca_display::cal_two_theta_event, event, cal

    nrois = cal.nrois
    calibration = cal.mca->get_calibration()

    case event.id of

        cal.widgets.do_fit: begin
            ; Find which ROIs should be used for the calibration
            use = where ((cal.roi[0:nrois-1].use eq 1), nuse)
            if (nuse lt 1) then begin
                t = dialog_message( /error, dialog=cal.widgets.base, $
                  'Must have at least one valid point for calibration')
                goto, done
            endif
            for i=0, nrois-1 do begin
                if (cal.energy[i] eq 0) or (cal.d_spacing[i] eq 0) then begin
                    cal.two_theta[i] = 0
                endif else begin
                    cal.two_theta[i] = 2.0 * asin(12.398 / $
                                (2.0*cal.energy[i]*cal.d_spacing[i]))/!dtor
                endelse
                widget_control, cal.widgets.two_theta[i], $
                    set_value=string(cal.two_theta[i], format='(f10.5)')
            endfor
            stat = moment(cal.two_theta[use])
            calibration.two_theta = stat[0]
            sdev = sqrt(stat[1])

            widget_control, cal.widgets.two_theta_fit, $
                set_value=string(calibration.two_theta, format='(f10.5)') + $
                          ' +- ' + string(sdev, format='(f7.5)')
            for i=0, nrois-1 do begin
                two_theta_diff = cal.two_theta[i] - calibration.two_theta
                widget_control, cal.widgets.two_theta_diff[i], $
                    set_value=string(two_theta_diff, format='(f10.5)')
            endfor
            cal.mca->set_calibration, calibration
        end

        cal.widgets.plot_cal: begin
            use = where ((cal.roi[0:nrois-1].use eq 1), nuse)
            if (nuse le 0) then goto, done
            energy = fltarr(nrois) & two_theta_diff = fltarr(nrois)
            for i=0, nrois-1 do begin
                energy[i] = cal.energy[i]
                two_theta_diff[i] = cal.two_theta[i] - calibration.two_theta
            endfor
            window, self.windows.energy_cal, title='MCA 2-theta Error'
            plot, energy[use], two_theta_diff[use], psym=1, charsize=1.5, $
                xtitle='Energy', ytitle='Two-theta error'
            oplot, energy[use], two_theta_diff[use]
        end

        cal.widgets.OK: begin
            self.foreground.mca->set_calibration, calibration
            self.foreground.mca->set_rois, cal.roi(0:nrois-1)
            self->update_spectrum, /rescale
            ; Update calibration text fields
            self->lmarker, self.display.lmarker
            self->rmarker, self.display.rmarker
            self->cursor,  self.display.cursor
            self->show_stats
            widget_control, event.top, /destroy
            obj_destroy, cal.mca
            return
        end

        cal.widgets.cancel: begin
            widget_control, event.top, /destroy
            obj_destroy, cal.mca
            return
        end

        else: begin
            for i=0, nrois-1 do begin
                if (event.id eq cal.widgets.label[i]) then begin
                    widget_control, event.id, get_value=str
                    cal.roi[i].label = str[0]
                endif else if (event.id eq cal.widgets.d_spacing[i]) then begin
                    widget_control, event.id, get_value=str
                    d_spacing = float(str[0])
                    cal.d_spacing[i] = d_spacing
                    widget_control, cal.widgets.d_spacing[i], $
                        set_value=string(d_spacing)
                endif else if (event.id eq cal.widgets.energy[i]) then begin
                    widget_control, event.id, get_value=str
                    energy = float(str[0])
                    cal.roi[i].energy = energy
                    widget_control, cal.widgets.energy[i], $
                        set_value=string(energy)
                endif else if (event.id eq cal.widgets.use_flag[i]) then begin
                    cal.roi[i].use = widget_info(event.id, /droplist_select)
                endif
            endfor
        end
    endcase
    done:
    widget_control, event.top, set_uvalue = cal, /no_copy
end


;**********************************************************************
pro mca_display::cal_two_theta

    nrois = self.foreground.nrois
    if (nrois lt 1) then begin
        t = dialog_message( /error, $
                'Must have at least one ROI to perform calibration')
        return
    endif

    widgets = { $
        base:           0L, $
        use_flag:       lonarr(nrois), $
        energy:         lonarr(nrois), $
        fwhm:           lonarr(nrois), $
        d_spacing:      lonarr(nrois), $
        label:          lonarr(nrois), $
        two_theta:      lonarr(nrois), $
        two_theta_diff: lonarr(nrois), $
        two_theta_fit:  0L, $
        do_fit:         0L, $
        plot_cal:       0L, $
        cancel:         0L, $
        OK:             0L  $
    }

    calibration = self.foreground.mca->get_calibration()
    roi = self.foreground.mca->get_rois()
    cal = {                             $
        mca:            obj_new('mca'),  $
        mca_display:    self,            $
        fwhm:           fltarr(nrois),   $
        fwhm_chan:      fltarr(nrois),   $
        centroid:       fltarr(nrois),   $
        energy:         fltarr(nrois),   $
        d_spacing:      fltarr(nrois),   $
        two_theta:      fltarr(nrois),   $
        nrois:          nrois,           $
        roi:            roi,             $
        widgets:        widgets          $
    }
    cal.mca->set_calibration, calibration

    ; Compute the centroid and FWHM of each ROI
    for i=0, nrois-1 do begin
        left = cal.roi[i].left
        right = cal.roi[i].right
        total_counts = self.foreground.data[left:right]
        n_sel        = right - left + 1L
        sel_chans    = left + indgen(n_sel)
        left_counts  = self.foreground.data[left]
        right_counts = self.foreground.data[right]
        bgd_counts   = left_counts + findgen(n_sel)/(n_sel-1) * $
                                 (right_counts - left_counts)
        net_counts   = total_counts - bgd_counts
        net          = total(net_counts)

        if ((net gt 0.) and (n_sel ge 3)) then begin
            mca_display_fit_gaussian, sel_chans, net_counts, amplitude, $
                                  centroid, fwhm
            cal.centroid[i] = centroid
            cal.fwhm_chan[i] = fwhm
        endif else begin
            cal.centroid[i] = (left + right)/2.
            cal.fwhm_chan[i] = right-left
        endelse
        cal.energy[i] = cal.mca->chan_to_energy(cal.centroid[i])
        cal.fwhm[i] = cal.mca->chan_to_energy(cal.centroid[i] + $
                                          cal.fwhm_chan[i]/2.) - $
                      cal.mca->chan_to_energy(cal.centroid[i] - $
                                          cal.fwhm_chan[i]/2.)
    endfor

    base = widget_base(title="MCA Calibrate Two-theta", /column)
    cal.widgets.base = base

    row = widget_base(base, /row, /frame)
    ; Determine height for all of the widgets in this row
    dummy = widget_text(base, xsize=6)
    geometry = widget_info(dummy, /geometry)
    widget_control, dummy, /destroy
    scr_ysize = geometry.scr_ysize
    scr_xsize = geometry.scr_xsize

    col = widget_base(row, /column)
    t = widget_label(col, value='ROI', font=self.fonts.label)
    for i=0, nrois-1 do begin
        t = widget_label(col, value=strtrim(string(i),2), scr_ysize=scr_ysize)
    endfor

    col = widget_base(row, /column)
    t = widget_label(col, value='Use?', font=self.fonts.label)
    for i=0, nrois-1 do begin
        cal.widgets.use_flag[i] = $
            widget_droplist(col, value=['No','Yes'], scr_ysize=scr_ysize)
        widget_control, cal.widgets.use_flag[i], $
                    set_droplist_select=cal.roi[i].use
    endfor

    col = widget_base(row, /column)
    t = widget_label(col, value='Energy', font=self.fonts.label)
    for i=0, nrois-1 do begin
        cal.widgets.energy[i] = $
            widget_text(col, /edit, value=string(cal.energy[i]), $
                        xsize=11)
    endfor

    col = widget_base(row, /column)
    t = widget_label(col, value='FWHM', font=self.fonts.label)
    for i=0, nrois-1 do begin
        cal.widgets.fwhm[i] = $
            widget_text(col, value=string(cal.fwhm[i]), $
                        xsize=11)
    endfor

    col = widget_base(row, /column)
    t = widget_label(col, value='D spacing', font=self.fonts.label)
    for i=0, nrois-1 do begin
        d = lookup_jcpds_line(cal.roi[i].label, path=self.file.jcpds_path)
        if (d ne 0.) then cal.roi[i].d_spacing = d
        cal.d_spacing[i] = cal.roi[i].d_spacing
        cal.widgets.d_spacing[i] = $
            widget_text(col, /edit, value=string(cal.d_spacing[i]), $
                        xsize=11)
    endfor

    col = widget_base(row, /column)
    t = widget_label(col, value='HKL', font=self.fonts.label)
    for i=0, nrois-1 do begin
        cal.widgets.label[i] = $
            widget_text(col, /edit, value=cal.roi[i].label, $
                        xsize=10)
    endfor

    col = widget_base(row, /column)
    t = widget_label(col, value='Two-theta', font=self.fonts.label)
    for i=0, nrois-1 do begin
        cal.widgets.two_theta[i] = $
            widget_text(col, value=string(cal.two_theta[i]), $
                        xsize=11)
    endfor

    col = widget_base(row, /column)
    t = widget_label(col, value='Two-theta diff.', font=self.fonts.label)
    for i=0, nrois-1 do begin
        cal.widgets.two_theta_diff[i] = $
            widget_text(col, value=string(0.0), $
                        xsize=11)
    endfor

    row = widget_base(base, /row, /frame)
    cal.widgets.do_fit    = widget_button(row, value='Compute 2-theta')
    cal.widgets.plot_cal  = widget_button(row, value='Plot 2-theta error')
    cal.widgets.two_theta_fit  = cw_field(row, title='Two-theta',           $
                                  value=calibration.two_theta,         $
                                  /column, /string, xsize=20,     $
                                  /return_events)
    row = widget_base(base, /row, /frame)
    cal.widgets.OK = widget_button(row, value="OK", $
                 font=self.fonts.big_button)
    cal.widgets.cancel = widget_button(row, value="Cancel", $
                 font=self.fonts.big_button)
    widget_control, base, set_uvalue=cal
    widget_control, base, /realize
    xmanager, "mca_display::cal_two_theta", base, $
              group_leader=self.widgets.base, $
              event="mca_display_cal_two_theta_event", /no_block
end


;*****************************************************************************
pro mca_display::show_roi, in_index
   if (self.foreground.nrois le 0) then return
   index = in_index > 0 < (self.foreground.nrois-1)
   left = self.foreground.roi[index].left
   right = self.foreground.roi[index].right
   middle = (left + right)/2.
   if (self.display.hmin gt left) or (self.display.hmax lt right) then begin
        range = self.display.hmax - self.display.hmin
        hmin = (middle - range/2.) < left
        hmax = (middle + range/2.) > right
        self.display.hmin = (hmin>0) < (self.foreground.nchans-1)
        self.display.hmax = (hmax > hmin) < (self.foreground.nchans-1)
        self->update_spectrum, /rescale
   endif
   self->lmarker, left
   self->rmarker, right
   ; Redraw left marker, since erasing old right marker can erase it too if they
   ; are in the same pixel
   self->lmarker, left
   self->cursor, middle
   self->show_stats
   widget_control, self.widgets.label_roi, $
            set_value=self.foreground.roi[index].label
end


;*****************************************************************************
pro mca_display::update_markers, object
    ; Redraws the position and counts for the markers or cursor
    ; object=0: cursor
    ; object=1: left marker
    ; object=2: right marker

    case object of
        0: begin
            chan = self.display.cursor
            pos_widget = self.widgets.cur_pos
            count_widget = self.widgets.cur_counts
        end
        1: begin
            chan = self.display.lmarker
            pos_widget = self.widgets.lm_pos
            count_widget = self.widgets.lm_counts
        end
        2: begin
            chan = self.display.rmarker
            pos_widget = self.widgets.rm_pos
            count_widget = self.widgets.rm_counts
        end
    endcase

    ; Get calibrated units

    if (self.foreground.valid) then $
        cal = self->chan_to_cal(chan) $
    else cal = chan

    case self.display.horiz_mode of
        0: begin   ; Channel
            str = string(cal, format = '(i9)')
            widget_control, pos_widget, set_value = str
        end

        1: begin   ; Energy
            str = string(cal, format = '(f9.3)')
            widget_control, pos_widget, set_value = str
        end

        2: begin   ; d-spacing
            str = string(cal, format = '(f9.3)')
            widget_control, pos_widget, set_value = str
        end
    endcase

    ; Update the counts on the screen
    str = string(self.foreground.data[chan], format = '(i9)')
    widget_control, count_widget, set_value = str
end


;*****************************************************************************
pro mca_display::lmarker, value, energy=energy, d_spacing=d_spacing

    if (keyword_set(energy)) then $
         chan = self.foreground.mca->energy_to_chan(value) $
    else if (keyword_set(d_spacing)) then $
         chan = self.foreground.mca->d_to_chan(value) $
    else chan = value

    ; Restore window and plot conversions
    wset, self.windows.plot
    !x = self.display.sys_x
    !y = self.display.sys_y
    !p = self.display.sys_p

    ; Need to erase present marker and update the text widgets
    t = convert_coord( self.display.lmarker, $
                       self.foreground.data[self.display.lmarker], $
                       /data, /to_normal)
    plots, [t[0], t[0]], [t[1]+.01, t[1]+.1], /normal, $
        color = self.colors.background
    ; Set the new marker position
    chan = chan > self.display.hmin < (self.display.hmax - 1)
    self.display.lmarker = chan
    self->update_markers, 1

    ; Move the right marker if necessary
    if self.display.rmarker le self.display.lmarker $
        then self->rmarker, self.display.lmarker + 1L

    ; Draw the new marker
    t = convert_coord( self.display.lmarker, $
                       self.foreground.data[self.display.lmarker], /data,$
                       /to_normal)
    plots, [t[0], t[0]], [t[1]+.01, t[1]+.1], /normal, $
           color = self.colors.markers

end



;*****************************************************************************
pro mca_display::rmarker, value, energy=energy, d_spacing=d_spacing

    if (keyword_set(energy)) then $
        chan = self.foreground.mca->energy_to_chan(value) $
    else if (keyword_set(d_spacing)) then $
        chan = self.foreground.mca->d_to_chan(value) $
    else $
        chan = value

    ; Restore window and plot conversions
    wset, self.windows.plot
    !x = self.display.sys_x
    !y = self.display.sys_y
    !p = self.display.sys_p

    ; Need to erase present marker and update the text widgets
    t = convert_coord( self.display.rmarker, $
            self.foreground.data[self.display.rmarker], /data, /to_normal)
    plots, [t[0], t[0]], [t[1]+.01, t[1]+.1], /normal, $
            color = self.colors.background
    ; Set the new marker position
    chan = chan > (self.display.hmin + 1) < self.display.hmax
    self.display.rmarker = chan
    ; Update the channel position on the screen
    self->update_markers, 2

    ; Move the left marker if necessary
    if (self.display.lmarker ge self.display.rmarker) $
        then self->lmarker, self.display.rmarker - 1L
    ; Draw the new marker
    t = convert_coord( self.display.rmarker, $
            self.foreground.data[self.display.rmarker], /data, /to_normal)
    plots, [t[0], t[0]], [t[1]+.01, t[1]+.1], /normal, $
            color = self.colors.markers

end



;*****************************************************************************
pro mca_display::cursor, value, energy=energy, d_spacing=d_spacing

    if (keyword_set(energy)) then $
        chan = self.foreground.mca->energy_to_chan(value) $
    else if (keyword_set(d_spacing)) then $
        chan = self.foreground.mca->d_to_chan(value) $
    else $
        chan = value

    ; Restore window and plot conversions
    wset, self.windows.plot
    !x = self.display.sys_x
    !y = self.display.sys_y
    !p = self.display.sys_p

    ; Need to erase present marker and update the text widgets
    t = convert_coord(self.display.cursor, $
            self.foreground.data[self.display.cursor], /data, /to_normal)
    plots, [t[0], t[0]], [t[1]+.01, t[1]+.15], /normal, $
            color = self.colors.background
    ; Set the new cursor position
    chan = chan > self.display.hmin < self.display.hmax
    self.display.cursor = chan
    ; Update the channel position on the screen
    self->update_markers, 0

    ; Draw the new cursor
    t = convert_coord( self.display.cursor, $
            self.foreground.data[self.display.cursor], /data, /to_normal)
    plots, [t[0], t[0]], [t[1]+.01, t[1]+.15], /normal, $
        color = self.colors.markers

end



;*****************************************************************************
pro mca_display::klm_markers, in_z

    ;  Displays X-ray lines of element with atomic number Z.  It attempts
    ;  to display the K, L, and M series lines in that order, depending
    ;  upon which series, if any, occurs within the energy range of the
    ;  display.

    series_symbol = ['K',  'L']
    low           = ['a1', 'a1']
    middle        = ['b1', 'b1']
    high          = ['b2', 'b2']

    ; Check that z is within bounds.
    z = (in_z > 1) < 100
    self.display.klm = z

    ; Display first series (K,L,M) with any line within this energy window.
    element = atomic_symbol(z)
    emin = self.foreground.mca->chan_to_energy(self.display.hmin)
    emax = self.foreground.mca->chan_to_energy(self.display.hmax)

    for i=0,1 do begin
        series = series_symbol[i]
        low_line    = lookup_xrf_line(element + ' ' + series+low[i])
        middle_line = lookup_xrf_line(element + ' ' + series+middle[i])
        high_line   = lookup_xrf_line(element + ' ' + series+high[i])
        if ((emin le low_line) and (emax ge low_line)) then begin
            self->cursor,  low_line,              /energy
            self->lmarker, middle_line>low_line,  /energy
            self->rmarker, high_line>middle_line, /energy
            series = series+low[i]+','+middle[i]+','+high[i]
            goto, done
        endif
    endfor

    ; No lines of this element occur within the present window
    center = (self.display.hmin + self.display.hmax) / 2.
    self->cursor,  center
    self->lmarker, center
    self->rmarker, center+1
    series = '(none)'

    done:
    widget_control, self.widgets.klm_label, $
            set_value=atomic_symbol(z) + ' ' + series
end

;*****************************************************************************
pro mca_display_fit_gaussian, sel_chans, net_counts, amplitude, centroid, fwhm

    ; Fits a peak to a Gaussian using a linearizing method
    ; Returns centroid and fwhm in channels
    ; TEMPORARY FIX - the centroid is computed wrong for very sharp (i.e.
    ; single channel peaks.  Doing things in double helps, but does not fix
    ; problem completely.
    center = double((sel_chans[0] + sel_chans[n_elements(sel_chans)-1])/2.)
    net = double(net_counts)>1.0
    weight = net^2
    ; TEMPORARY FIT - THE DISTRIBUTION VERSION OF POLYFITW HAS A BUG
    ; USE OUR VERSION FOR NOW UNTIL IT IS FIXED
    fic = cars_polyfitw(sel_chans-center, $
               alog(net), $
               weight,2)
    fic[2] = fic[2] < (-.001)  ; Protect against divide by 0
    ; We don't currently need the amplitude for display, and it is causing
    ; arithmetic overflows, etc.  Do more range checking before turning it
    ; back on.
    ;amplitude = exp(fic[0] - fic[1]^2/(4.*fic[2]))
    centroid  = center - fic[1]/(2.*fic[2])
    sigma     = sqrt(-1/(2.*fic[2]))
    fwhm      = 2.35482 * sigma
end


;*****************************************************************************
pro mca_display::new_hmin, widgets, chan
    chan = chan > 0 < (self.foreground.nchans-2)
    str = string(chan, format='(i9)')
    widget_control, widgets.hmin_c, set_value=str
    str = string(self.foreground.mca->chan_to_energy(chan), format='(f9.3)')
    widget_control, widgets.hmin_e, set_value=str
    self.display.hmin=chan
    ; Change hmax if necessary
    if (self.display.hmax le self.display.hmin) then begin
        self->new_hmax, widgets, chan+1
    endif
end

;*****************************************************************************
pro mca_display::new_hmax, widgets, chan
    chan = chan > 1 < (self.foreground.nchans-1)
    str = string(chan, format='(i9)')
    widget_control, widgets.hmax_c, set_value=str
    str = string(self.foreground.mca->chan_to_energy(chan), format='(f9.3)')
    widget_control, widgets.hmax_e, set_value=str
    self.display.hmax=chan
    ; Change hmin if necessary
    if (self.display.hmin ge self.display.hmax) then begin
        self->new_hmin, widgets, chan-1
    endif
end

;*****************************************************************************
pro mca_display::new_vmin, widgets, counts
    self.display.vmin = counts > 0
    str = string(self.display.vmin, format='(i9)')
    widget_control, widgets.vmin, set_value=str
    self.display.vmax= self.display.vmax > (self.display.vmin+1)
    str = string(self.display.vmax, format='(i9)')
    widget_control, widgets.vmax, set_value=str
    ; Change vmax if necessary
    if (self.display.vmax le self.display.vmin) then begin
        self->new_vmax, widgets, counts+1
    endif
end

;*****************************************************************************
pro mca_display::new_vmax, widgets, counts
    self.display.vmax = counts > 0
    str = string(self.display.vmax, format='(i9)')
    widget_control, widgets.vmax, set_value=str
    self.display.vmin= self.display.vmin < (self.display.vmax-1)
    str = string(self.display.vmin, format='(i9)')
    widget_control, widgets.vmin, set_value=str
    ; Change vmin if necessary
    if (self.display.vmin ge self.display.vmax) then begin
        self->new_vmin, widgets, counts-1
    endif
end


;*****************************************************************************
pro mca_display_display_setup_event, event

    widget_control, event.top, get_uvalue=top
    top.mca_display->display_setup_event, event, top.widgets
end


;*****************************************************************************
pro mca_display::display_setup_event, event, widgets

    case event.id of

        widgets.rescale: begin
            ; The main program has sent a rescale event
            ; Update the hmin, hmax, vmin, vmax values
            self->new_hmin, widgets, self.display.hmin
            self->new_hmax, widgets, self.display.hmax
            self->new_vmin, widgets, self.display.vmin
            self->new_vmax, widgets, self.display.vmax
        end

        widgets.update: begin
            widget_control, event.id, get_uvalue=uvalue
            index = widget_info(event.id, /droplist_select)
            time = uvalue[index]
            self.display.update_time = time
        end

        widgets.psym: begin
            select = widget_info(event.id, /droplist_select)
            self.display.psym = (self.display.psym_choices)[select]
        end

        widgets.hmin_c:  begin
            widget_control, event.id, get_value=value
            self->new_hmin, widgets, long(value[0])
        end

        widgets.hmin_e:  begin
            widget_control, event.id, get_value=value
            self->new_hmin, widgets, $
                self.foreground.mca->energy_to_chan(float(value[0]))
        end

        widgets.hmax_c:  begin
            widget_control, event.id, get_value=value
            self->new_hmax, widgets, long(value[0])
        end

        widgets.hmax_e:  begin
            widget_control, event.id, get_value=value
            self->new_hmax, widgets, $
                self.foreground.mca->energy_to_chan(float(value[0]))
        end

        widgets.vmin:    begin
            widget_control, event.id, get_value=value
            self->new_vmin, widgets, float(value[0])
        end

        widgets.vmax:    begin
            widget_control, event.id, get_value=value
            self->new_vmax, widgets, float(value[0])
        end

        widgets.vauto: begin
            widget_control, event.id, get_uvalue=uvalue
            index = widget_info(event.id, /droplist_select)
            self.display.vauto = uvalue[index]
            widget_control, widgets.vmin, sensitive=(self.display.vauto eq 0)
            widget_control, widgets.vmax, sensitive=(self.display.vauto eq 0)
        end

        widgets.color_index: begin
            color = widget_info(widgets.color_index, /droplist_select)
            tvlct, /get, r, g, b
            widget_control, widgets.rgb, set_value=[r[color],g[color],b[color]]
            wset, widgets.draw
            tv, bytarr(50,50)+byte(color)
        end

        widgets.rgb: begin
            color = widget_info(widgets.color_index, /droplist_select)
            tvlct, event.r, event.g, event.b, color
            wset, widgets.draw
            tv, bytarr(50,50)+byte(color)
        end

        widgets.exit: begin
            widget_control, event.top, /destroy
            return
        end

        else:   t = dialog_message(/warn, "Unknown button pressed")

    endcase
    if (event.id ne widgets.rescale) then self->update_spectrum, /rescale
end



;**********************************************************************
pro mca_display::display_setup, group=group

    widgets = { $
        base:           0L, $
        rescale:        0L, $
        update:         0L, $
        psym:           0L, $
        hmin_c:         0L, $
        hmin_e:         0L, $
        hmax_c:         0L, $
        hmax_e:         0L, $
        vmin:           0L, $
        vmax:           0L, $
        vauto:          0L, $
        color_index:    0L, $
        rgb:            0L, $
        draw:           0L, $
        exit:           0L $
    }

    base = widget_base(title="MCA Display Control",/row, $
                    group=self.widgets.base)
    widgets.base = base
    widgets.rescale = base
    self.widgets.rescale = base  ; The main program uses this to send events
    choices=[.1, .2, .5, 1.0, 2.0, 5.0]
    col1 = widget_base(base, /column, /frame)
    row = widget_base(col1, /row, /frame)
    t = widget_label(row, value='Display Update Rate:')
    rates = [.1, .2, .5, 1.0, 2.0, 5.0]

    widgets.update = widget_droplist( row, $
        value = ['.1 second', '.2 second', '.5 second', '1 second', $
                 '2 seconds', '5 seconds'], uvalue=rates)
    index = where(self.display.update_time eq rates)
    if (index[0] eq -1) then index=3
    widget_control, widgets.update, set_droplist_select=index[0]

    row = widget_base(col1, /row, /frame)
    t = widget_label(row, value = 'Line style:')
    widgets.psym  = widget_droplist(row, $
                    value = self.display.psym_names, $
                    font = self.fonts.button)
    select = where (self.display.psym eq self.display.psym_choices)
    widget_control, widgets.psym, set_droplist_select=select[0]

    col = widget_base(col1, /column, /frame)
    t = widget_label(col, value='Horizontal', font=self.fonts.big_label)
    row = widget_base(col, /row)
    col = widget_base(row, /column)
    temp = widget_text(col, /edit, xsize=10)
    geometry = widget_info(temp, /geometry)
    xsize = geometry.scr_xsize
    ysize = geometry.scr_ysize
    widget_control, temp, /destroy
    t = widget_label(col, value=' ', scr_ysize=ysize)
    t = widget_label(col, value='Channel', scr_ysize=ysize)
    t = widget_label(col, value='Energy', scr_ysize=ysize)
    col = widget_base(row, /column)
    t = widget_label(col, value='Minimum', scr_xsize=xsize, scr_ysize=ysize)
;  The scr_ysize in the calls to widget_text here and below is not needed
;  (it will be the default) and does not work on Windows NT in IDL 5.0
    widgets.hmin_c = widget_text(col, /edit, scr_xsize=xsize)
    widgets.hmin_e = widget_text(col, /edit, scr_xsize=xsize)
    col = widget_base(row, /column)
    t = widget_label(col, value='Maximum', scr_xsize=xsize, scr_ysize=ysize)
    widgets.hmax_c = widget_text(col, /edit, scr_xsize=xsize)
    widgets.hmax_e = widget_text(col, /edit, scr_xsize=xsize)
    self->new_hmin, widgets, self.display.hmin
    self->new_hmax, widgets, self.display.hmax

    col = widget_base(col1, /column, /frame)
    t = widget_label(col, value='Vertical', font=self.fonts.big_label)
    row = widget_base(col, /row)
    col = widget_base(row, /column)
    t = widget_label(col, value='Scaling')
    widgets.vauto = widget_droplist(col, value=['Manual', 'Auto'], uvalue=[0,1])
    widget_control, widgets.vauto, set_droplist_select=self.display.vauto
    col = widget_base(row, /column)
    t = widget_label(col, value='Minimum')
    widgets.vmin = widget_text(col, /edit, xsize=10)
    col = widget_base(row, /column)
    t = widget_label(col, value='Maximum')
    widgets.vmax = widget_text(col, /edit, xsize=10)
    self->new_vmin, widgets, self.display.vmin
    widget_control, widgets.vmin, sensitive=(self.display.vauto eq 0)
    widget_control, widgets.vmax, sensitive=(self.display.vauto eq 0)

    widgets.exit = widget_button(col1, value="Exit")

    col = widget_base(base, /column, /frame)
    t = widget_label(col, value='Colors', font=self.fonts.big_label)
    row = widget_base(col, /row)
    labels = ['Background', 'Markers and cursor', 'Foreground spectrum', $
              'Background spectrum', 'ROIs']
    color_indices = [0,1,2,3,4]
    t = widget_label(row, value='Object:')
    widgets.color_index = widget_droplist(row, $
            value=labels, uvalue=color_indices)
    widgets.draw = widget_draw(row, xsize=50, ysize=50)
    widgets.rgb = cw_rgbslider(col, /drag)

    widget_control, base, /realize
    widget_control, widgets.draw, get_value=t
    widgets.draw = t
    widget_control, base, set_uvalue = {mca_display: self, widgets: widgets}
    xmanager, "mca_display::display_setup", base, group=self.widgets.base, $
        event="mca_display_display_setup_event", /no_block
end



;*****************************************************************************
pro mca_display_event, event ; event processing for MCA application

    widget_control, event.top, get_uvalue = mca_display
    mca_display->event, event
end

;*****************************************************************************
pro mca_display::event, event ; event processing for MCA application

    case event.id of

        self.widgets.timer:  begin
            redraw_needed = 0
            stats_changed = 0
            if (self.windows.mouse_button eq 0L) then begin
                if obj_valid(self.foreground.mca) and $
                            (self.foreground.is_detector) then begin
                    acqg = self.foreground.mca->get_acquire_status( $
                                       /check_new, new_flag)
                    if (new_flag) then begin
                        stats_changed = 1
                        self.display.current_acqg = acqg
                        if (self.display.current_acqg) then begin
                            self.options.save_done = 0
                            widget_control, self.widgets.on, sensitive = 0
                            widget_control, self.widgets.off, sensitive = 1
                        endif else begin
                            if (self.display.prev_acqg) then begin
                                ; Acquisition just completed
                                if (self.options.autosave) then begin
                                    self->save_file, self.file.next_filename
                                endif
                                if (self.options.autorestart) then begin
                                    self.foreground.mca->erase
                                    self.foreground.mca->acquire_on
                                endif
                            endif
                            widget_control, self.widgets.on, sensitive = 1
                            widget_control, self.widgets.off, sensitive = 0
                        endelse
                    endif
                    self.display.prev_acqg = self.display.current_acqg
                    temp=self.foreground.mca->get_elapsed(/check_new, new_flag)
                    if (new_flag) then begin
                        self.foreground.elapsed = temp
                        stats_changed = 1
                    endif
                    temp=self.foreground.mca->get_data(/check_new, new_flag)
                    if (new_flag) then begin
                        self.foreground.data=temp
                        ; These values are used for computing counts/second
                        ; in show_stats
                        self.display.prev_time = self.display.current_time
                        self.display.current_time = $
                                        self.foreground.elapsed.read_time
                        self.display.prev_counts = self.display.current_counts
                        self.display.prev_bgd = self.display.current_bgd
                        redraw_needed = 1
                    endif
                endif else begin  ; Foreground is not detector
                    self.display.current_acqg = 0
                endelse

                if obj_valid(self.background.mca) and $
                            (self.background.is_detector) then begin
                    temp=self.background.mca->get_data(/check_new, new_flag)
                    if (new_flag) then begin
                        self.background.data=temp
                        redraw_needed = 1
                    endif
                    temp=self.background.mca->get_elapsed(/check_new, new_flag)
                    if (new_flag) then begin
                        self.background.elapsed = temp
                        stats_changed = 1
                    endif
                endif
            endif else begin
                stats_changed = 1  ; User is moving cursor or markers
            endelse
            if (redraw_needed) then self->update_spectrum
            if (stats_changed) then self->show_stats

            widget_control, self.widgets.timer, timer = self.display.update_time
        end

        self.widgets.plot: begin
            case event.type of
                0L:   begin   ; Button press event
                    widget_control, self.widgets.plot, draw_motion_events = 1L
                    self.windows.mouse_button = event.press
                end
                1L:   begin   ; Button release event
                    widget_control, self.widgets.plot, draw_motion_events = 0L
                    widget_control, event.id, /clear_events
                    self.windows.mouse_button = 0L
                    ; Repaint screen, since moving cursor and markers can
                    ; cause screen "damage"
                    self->update_spectrum
                    self->show_stats
                end
                else:
            endcase
            t = convert_coord( event.x, event.y, /device, /to_data)
            chan = long( t[0]) > 0L < (self.foreground.nchans - 1L)
            case self.windows.mouse_button of
                0L:
                1L: self->cursor,  chan     ; Left button
                2L: self->lmarker, chan     ; Middle button
                4L: self->rmarker, chan     ; Right button
                5L: self->lmarker, chan     ; Left + right = middle emulation
                else:
            endcase
        end

        self.widgets.foreground_open_det:   self->open_det

        self.widgets.foreground_open_file: begin
            file = dialog_pickfile( title = 'Select data file', $
                             path = self.file.filepath,  $
                             /must_exist,                $
                             filter=['*.*', '*.mca'],    $
                             get_path = path)
            if (file ne '') then begin
                ; Cancel button returns null string
                self.file.filepath = path
                self.file.filename = file
                self->open_file, file
            endif
        end

        self.widgets.save_as: begin
            file = dialog_pickfile(/write, $
                             path = self.file.filepath,  $
                             get_path = path)
            if (file ne '') then begin
                ; Cancel button returns null string
                self.file.filepath = path
                self.file.filename = file
                self->save_file, file
            endif
        end

        self.widgets.save_next: begin
            self->save_file, self.file.next_filename
        end

        self.widgets.background_open_det:   self->open_det, /background

        self.widgets.background_open_file: begin
            file = dialog_pickfile( title = 'Select data file', $
                             path = self.file.filepath,  $
                             /must_exist,                $
                             get_path = path)
            if (file ne '') then begin
                ; Cancel button returns null string
                self.file.filepath = path
                self.file.filename = file
                self.background.name = file
                self->open_file, file, /background
            endif
        end

        self.widgets.background_close:  begin
            obj_destroy, self.background.mca
            self.background.valid = 0
            self.background.is_detector = 0
            self->new_inputs
        end

        self.widgets.swap_foreground:  begin
            temp = self.foreground
            self.foreground = self.background
            self.background = temp
            self->update_spectrum, /rescale
            self->show_stats
            self->new_inputs
        end

        self.widgets.new_window_tiny:   begin
            t = obj_new('mca_display', font_size=0, parent=self)
        end

        self.widgets.new_window_small:   begin
            t = obj_new('mca_display', font_size=1, parent=self)
        end

        self.widgets.new_window_medium:   begin
            t = obj_new('mca_display', font_size=2, parent=self)
        end

        self.widgets.new_window_large: begin
            t = obj_new('mca_display', font_size=3, parent=self)
        end

        self.widgets.file_options:  self->file_options

        self.widgets.print_setup:   t = dialog_printersetup()

        self.widgets.print:   self->print

        self.widgets.exit: begin
            widget_control, self.widgets.base, /destroy
            return
        end

        self.widgets.presets: self->control_presets

        self.widgets.calibrate_energy: self->cal_energy

        self.widgets.calibrate_two_theta: self->cal_two_theta

        self.widgets.display_setup:  self->display_setup

        self.widgets.display_jcpds:  self->display_jcpds

        self.widgets.fit_peaks:  self->peak_fit

        self.widgets.help:  begin
            command = getenv('MCA_HELP_COMMAND')
            if (command eq '') then begin
                t = dialog_message( /error,                      $
                        dialog = self.widgets.base, $
                        'Environment variable MCA_HELP_COMMAND not defined')
            endif else begin
                spawn, command
            endelse
        end

        self.widgets.on: begin
            self.foreground.mca->acquire_on
        end

        self.widgets.off:   begin
            self.foreground.mca->acquire_off
        end

        self.widgets.erase: begin
            if ((not self.options.save_done) and (self.options.warn_erase)) $
                    then begin
                reply = dialog_message( /question, dialog = self.widgets.base, $
                'Warning - data have not been saved.  Erase anyway? ')
                if (reply eq 'No') then return
            endif
            self.foreground.mca->erase
            self.options.save_done = 1
            self->update_spectrum
        end


        self.widgets.add_roi:  begin
            widget_control, /hourglass
            widget_control, self.widgets.label_roi, get_value = label
            roi = {MCA_ROI}
            roi.left = self.display.lmarker
            roi.right = self.display.rmarker
            roi.label = strtrim(label[0],2)
            roi.use = 1
            status = self.foreground.mca->add_roi(roi)
            if (status lt 0) then t = dialog_message('Too many ROIs')
            self.foreground.roi = self.foreground.mca->get_rois(roi_info)
            self.foreground.nrois = roi_info.nrois
            index = self.foreground.mca->find_roi(roi.left, roi.right)
            if (index ge 0) then self.display.current_roi = index
            self->update_spectrum
        end

        self.widgets.del_roi:  begin
            widget_control, /hourglass
            roi = self.foreground.mca->find_roi(self.display.lmarker, $
                                           self.display.rmarker)
            self.foreground.mca->del_roi, roi
            self.foreground.roi = self.foreground.mca->get_rois(roi_info)
            self.foreground.nrois = roi_info.nrois
            self.display.current_roi = (self.display.current_roi-1) > 0
            self->update_spectrum
        end

        self.widgets.clear_roi:  begin
            widget_control, /hourglass
            self.foreground.mca->set_rois
            self.foreground.roi = self.foreground.mca->get_rois(roi_info)
            self.foreground.nrois = roi_info.nrois
            self->update_spectrum
        end

        self.widgets.label_roi: begin
            if (self.foreground.nrois gt 0) then begin
                ; If the markers are on the current ROI then
                ; change the label of the current ROI
                roi = self.display.current_roi
                if ((self.display.lmarker eq $
                            self.foreground.roi[roi].left) and $
                        (self.display.rmarker eq $
                            self.foreground.roi[roi].right)) then begin
                    widget_control, event.id, get_value=value
                    self.foreground.roi[roi].label = strtrim(value[0],2)
                    self.foreground.mca->set_rois, $
                            self.foreground.roi(0:self.foreground.nrois-1)
                endif
            endif
        end

        self.widgets.next_roi: begin
            if self.foreground.nrois gt 0L then begin
                self.display.current_roi = self.display.current_roi + 1L
                if self.display.current_roi ge self.foreground.nrois $
                    then self.display.current_roi = 0L
                self->show_roi, self.display.current_roi
            endif
        end

        self.widgets.prev_roi: begin
            if self.foreground.nrois gt 0L then begin
                self.display.current_roi = self.display.current_roi - 1L
                if self.display.current_roi lt 0L then $
                    self.display.current_roi = self.foreground.nrois - 1L
                self->show_roi, self.display.current_roi
            endif
        end


        self.widgets.klm_label:  begin
            widget_control, event.id, get_value=value
            z = atomic_number(value[0])
            if (z ne 0) then begin
                self.display.klm = z
                self->klm_markers, z
            endif
        end

        self.widgets.klm_up:   begin
            self->klm_markers, self.display.klm + 1
        end

        self.widgets.klm_down: begin
            self->klm_markers, self.display.klm - 1
        end

        self.widgets.zoom_up: begin
            range = self.display.hmax-self.display.hmin
            t = (range/4) > 5       ; Always display at least 10 channels
            self.display.hmin = ((self.display.cursor - t) > 0) < $
                                    (self.foreground.nchans-1)

            self.display.hmax = ((self.display.cursor + t) > 0) < $
                                    (self.foreground.nchans-1)
            self->update_spectrum, /rescale
        end

        self.widgets.zoom_down: begin
            range = self.display.hmax-self.display.hmin
            t = range
            self.display.hmin = ((self.display.cursor - t) > 0) < $
                                    (self.foreground.nchans-1)

            self.display.hmax = ((self.display.cursor + t) > 0) < $
                                    (self.foreground.nchans-1)
            self->update_spectrum, /rescale
        end

        self.widgets.shift_up: begin
            range = self.display.hmax-self.display.hmin
            t = (range/2)
            self.display.hmax = ((self.display.hmax + t) > 0) < $
                                    (self.foreground.nchans-1)
            self.display.hmin = ((self.display.hmax - range) > 0) < $
                                    (self.foreground.nchans-1)
            self->update_spectrum, /rescale
        end

        self.widgets.shift_down: begin
            range = self.display.hmax-self.display.hmin
            t = (range/2)
            self.display.hmin = ((self.display.hmin - t) > 0) < $
                                    (self.foreground.nchans-1)
            self.display.hmax = ((self.display.hmin + range) > 0) < $
                                    (self.foreground.nchans-1)
            self->update_spectrum, /rescale
        end

        self.widgets.lin_log: begin
            self.display.vlog = widget_info(event.id, /droplist_select)
            self->update_spectrum, /rescale
        end

        self.widgets.horiz_mode: begin
            self.display.horiz_mode = widget_info(event.id, /droplist_select)
            ; Update calibration text fields
            self->lmarker, self.display.lmarker
            self->rmarker, self.display.rmarker
            self->cursor,  self.display.cursor
            self->show_stats
        end

        self.widgets.lm_pos:   begin
            widget_control, event.id, get_value = value
            case self.display.horiz_mode of
                0: self->lmarker, long(value[0])
                1: self->lmarker, float(value[0]),/energy
                2: self->lmarker, float(value[0]),/d_spacing
            endcase
            self->show_stats
        end

        self.widgets.rm_pos:   begin
            widget_control, event.id, get_value = value
            case self.display.horiz_mode of
                0: self->rmarker, long(value[0])
                1: self->rmarker, float(value[0]),/energy
                2: self->rmarker, float(value[0]),/d_spacing
            endcase
            self->show_stats
        end

        self.widgets.cur_pos:  begin
            widget_control, event.id, get_value = value
            case self.display.horiz_mode of
                0: self->cursor, long(value[0])
                1: self->cursor, float(value[0]),/energy
                2: self->cursor, float(value[0]),/d_spacing
            endcase
        end

        else: t = dialog_message(/warning, "Unknown button pressed")

    endcase
end



;*****************************************************************************
pro mca_display::update_spectrum, rescale = rescale

    ; We do all plotting into an offscreen pixmap, which is then copied
    ; into the visible onscreen window. This eliminates annoying flicker.
    ; Restore system plot variables - necessary to prevent other windows
    ; from interfering with the oplot command.  We should test to see if
    ; is more efficient to do this, or to simply always use the plot
    ; command.
    !x = self.display.sys_x
    !y = self.display.sys_y
    !p = self.display.sys_p
    wset, self.windows.pixmap
    erase, self.colors.background
    if (self.display.vauto) then begin
        fmax = max(self.foreground.data[self.display.hmin:self.display.hmax], $
                  min = fmin)
        if (self.background.valid) then begin
            bmax = max(self.background.data[self.display.hmin: $
                                            self.display.hmax], min = bmin)
            fmax = bmax > fmax
            fmin = bmin < fmin
        endif
        ynorm = convert_coord([0,0], [fmax,fmin], /data, /to_normal)
        if ((ynorm[1,0] gt 0.9) or $
            (ynorm[1,0] lt 0.5) or $
            (ynorm[1,1] gt 0.3) or $
            (ynorm[1,1] lt 0.025)) then begin
              ydata = convert_coord([0,0], [ynorm[1,0]*1.3, ynorm[1,1]], $
                        /normal, /to_data)
              self.display.vmax = ydata[1,0] > fmax
              self.display.vmin = ydata[1,1] < fmin
              rescale = 1
        endif
    endif

    if (keyword_set(rescale)) then begin
        ; Copy data from objects since it might have changed externally
        if obj_valid(self.foreground.mca) then begin
            self.foreground.data = self.foreground.mca->get_data()
        endif else begin
            self.foreground.data = 0
        endelse
        if obj_valid(self.background.mca) then begin
            self.background.data = self.background.mca->get_data()
        endif else begin
            self.background.data = 0
        endelse
        if (self.display.vlog) then begin
            self.display.vmin = self.display.vmin > 0.5
            self.display.vmax = self.display.vmax > 100
        endif else begin
            self.display.vmin = self.display.vmin > 0
            self.display.vmax = self.display.vmax > 10
        endelse
        self.display.hmax = (self.display.hmax) < (self.foreground.nchans-1)
        plot, self.foreground.data[0L : self.foreground.nchans-1L] > $
            self.display.vmin,                        $ ; show zeroes
            ylog = self.display.vlog, $
            yrange = [self.display.vmin, self.display.vmax], $
            xrange = [self.display.hmin-1, self.display.hmax+1], $
            color = self.colors.foreground_spectrum, psym = self.display.psym, $
            xstyle = 5, ystyle = 5, position = [0,0.02,1,1]
        ; Copy the system plot variables
        self.display.sys_x = !x
        self.display.sys_y = !y
        self.display.sys_p = !p
        ; Send a widget event to the plot setup menu if it exists
        rescale=self.widgets.rescale
        if (widget_info(rescale, /valid)) then begin
            widget_control, rescale, $
                send_event={mca_rescale_event, $
                            ID:rescale, $
                            TOP: 0L, $
                            HANDLER: 0L}
        endif
    endif else begin
        oplot, self.foreground.data[0L : self.foreground.nchans-1L] > $
            self.display.vmin,                        $ ; show zeroes
            color = self.colors.foreground_spectrum, psym = self.display.psym
    endelse

    if (self.background.valid) then begin
        oplot, self.background.data[0L : self.background.nchans-1L] > $
            self.display.vmin,                        $ ; show zeroes
            color = self.colors.background_spectrum, psym = self.display.psym
    endif

    ; Draw markers and cursor w/o writing to text widgets
    ; The following code must be consistent with that in the individual
    ; routines. We don't put it there for efficiency.
    t = convert_coord( self.display.cursor, $
            self.foreground.data[self.display.cursor], /data, /to_normal)
    plots, [t[0], t[0]], [t[1]+.01, t[1]+.15], $
                        /normal, color = self.colors.markers
    t = convert_coord( self.display.lmarker, $
            self.foreground.data[self.display.lmarker], /data, /to_normal)
    plots, [t[0], t[0]], [t[1]+.01, t[1]+.1], $
                        /normal, color = self.colors.markers
    t = convert_coord( self.display.rmarker, $
            self.foreground.data[self.display.rmarker], /data,  /to_normal)
    plots, [t[0], t[0]], [t[1]+.01, t[1]+.1], $
                        /normal, color = self.colors.markers

    ; Draw ROIs
    for i = 0, self.foreground.nrois-1 do begin
        left = self.foreground.roi[i].left
        right = self.foreground.roi[i].right
; MN 20 Mar 2002: add check for valid ROI boundaries
        if (right gt left) then begin
            chans = left + indgen(right-left+1)
            oplot, chans, self.foreground.data[left:right] > $
              self.display.vmin,                $ ; show zeroes
              color = self.colors.roi, psym = self.display.psym
        endif
    endfor
    ;
    ; Now copy the pixmap into the visible window.
    wset, self.windows.plot
    device, copy = [0L,0L,self.display.xsize,self.display.ysize, $
                    0L,0L,self.windows.pixmap]
end



;*****************************************************************************
pro mca_display::show_stats

    if (not self.foreground.valid) then return

    ; Display statistics on region between left and right markers
    ;
    left = self.display.lmarker
    right = self.display.rmarker
    total_counts = self.foreground.data[left:right]
    tot  = total(total_counts)
    n_sel        = right - left + 1L
    sel_chans    = left + indgen(n_sel)
    left_counts  = self.foreground.data[left]
    right_counts = self.foreground.data[right]
    bgd_counts   = left_counts + findgen(n_sel)/(n_sel-1) * $
                                 (right_counts - left_counts)
    bgd = total(bgd_counts)
    net_counts   = total_counts - bgd_counts
    net          = total(net_counts)

    self.display.current_counts = tot
    self.display.current_bgd = bgd

    ; Total Counts
    self.display.current_counts = tot
    str = string(tot, format = '(i9)')
    widget_control, self.widgets.total_cts, set_value = str
    ;
    ; Net Counts
    str = string(tot - bgd, format = '(i9)')
    widget_control, self.widgets.net_cts, set_value = str

    ; Counts/second
    ; If acquisition is in progress then use instantaneous counts/sec, else
    ; use integrated counts/second
    total_cps = 0.
    net_cps = 0.
    if (self.display.current_acqg) then begin
        delta_t = self.display.current_time - self.display.prev_time
        if (delta_t gt 0) then begin
            total_cps = (self.display.current_counts - self.display.prev_counts) $
                        /delta_t
            net_cps = (self.display.current_counts - self.display.prev_counts - $
                      self.display.current_bgd + self.display.prev_bgd) / delta_t
        endif
    endif else begin
        if (self.foreground.elapsed.real_time gt 0.) then begin
            total_cps = self.display.current_counts / $
                                (self.foreground.elapsed.real_time)
            net_cps = (self.display.current_counts - self.display.current_bgd) / $
                                (self.foreground.elapsed.real_time)
        endif
    endelse
    str = string(total_cps, format = '(f9.1)')
    widget_control, self.widgets.total_cps, set_value = str
    str = string(net_cps, format = '(f9.1)')
    widget_control, self.widgets.net_cps, set_value = str

    ; Peak centroid and FWHM
    if ((net gt 0.) and (n_sel ge 3)) then begin
        mca_display_fit_gaussian, sel_chans, net_counts, amplitude, $
                             centroid, fwhm
    endif else begin
        centroid = (left + right)/2.
        fwhm = right - left
    endelse
    cal = self->chan_to_cal(centroid)
    str = string(cal, format = '(f9.3)')
    widget_control, self.widgets.center_pos, set_value = str
    ; To calculate FWHM in energy is a little tricky because of possible
    ; quadratic calibration term.
    cal = self->chan_to_cal(centroid+fwhm/2.) - $
          self->chan_to_cal(centroid-fwhm/2.)
    str = string(abs(cal), format = '(f9.3)')
    widget_control, self.widgets.fwhm_pos, set_value = str

    ; Marker  and cursor counts
    str = string(self.foreground.data[left], format = '(i9)')
    widget_control, self.widgets.lm_counts, set_value = str
    str = string(self.foreground.data[self.display.cursor], format = '(i9)')
    widget_control, self.widgets.cur_counts, set_value = str
    str = string(self.foreground.data[right], format = '(i9)')
    widget_control, self.widgets.rm_counts, set_value = str

    ; Elapsed live time, real time and counts
    str = string(self.foreground.elapsed.live_time, format = '(f8.2)')
    widget_control, self.widgets.elive, set_value = str
    str = string(self.foreground.elapsed.real_time, format = '(f8.2)')
    widget_control, self.widgets.ereal, set_value = str

end



;*****************************************************************************
pro mca_display::create_widgets

    title = 'Channel Access MCA version ' + self.options.version

    ; Define some values used in laying out widgets
    frame_size=3
    space=1
    xpad=1
    ypad=1
    base = widget_base( title = title, mbar = mbar, /column, resource = 'mca', $
                        space=space, xpad=xpad, ypad=ypad, uvalue=self)

    widget_control, base, default_font = self.fonts.default

    self.widgets.base  = base
    self.widgets.timer = base

    row            = widget_base(base, /row, space=space, xpad=xpad, ypad=ypad)
    control_column = widget_base(row, /column, space=space, xpad=xpad, $
                                 ypad=ypad)
    data_column    = widget_base(row, /column, space=space, xpad=xpad, $
                                 ypad=ypad)

    file           = widget_button( mbar, /menu,                 $
                                font = self.fonts.big_menu,  $
                                value = 'File      ')
    foreground     = widget_button( file, value = 'Foreground', /menu)
    self.widgets.foreground_open_det = widget_button( foreground, $
                                            value = 'Open detector . . .')
    self.widgets.foreground_open_file = widget_button( foreground, $
                                            value = 'Read file . . .')
    background     = widget_button( file, value = 'Background', /menu)
    self.widgets.background_open_det = widget_button( background, $
                                            value = 'Open detector . . .')
    self.widgets.background_open_file = widget_button( background, $
                                            value = 'Read file . . .')
    self.widgets.background_close     = widget_button( background, $
                                            value = 'Close')
    self.widgets.swap_foreground     = widget_button( file, $
                                    value = 'Swap Foreground<->Background')
    self.widgets.save_next           = widget_button( file, /dynamic, $
                                            value = 'Save Next = ' + $
                                            self.file.next_filename)
    self.widgets.save_as        = widget_button( file, value = 'Save As. . .')
    self.widgets.new_window     = widget_button( file, $
                                            value = 'New MCA window', /menu)

    self.widgets.new_window_tiny    = widget_button( self.widgets.new_window, $
                                            value = 'Tiny')

    self.widgets.new_window_small   = widget_button( self.widgets.new_window, $
                                            value = 'Small')

    self.widgets.new_window_medium  = widget_button( self.widgets.new_window, $
                                            value = 'Medium')

    self.widgets.new_window_large   = widget_button( self.widgets.new_window, $
                                            value = 'Large')

    self.widgets.print_setup        = widget_button( file, $
                                            value = 'Print setup . . .')
    self.widgets.print              = widget_button( file, $
                                            value = 'Print . . .')
    self.widgets.file_options       = widget_button( file, $
                                            value = 'Preferences . . .')
    self.widgets.exit               = widget_button( file, value = 'Exit')

    cntl                            = widget_button( mbar, /menu,             $
                                            font = self.fonts.big_menu,  $
                                            value = 'Control      ')
    self.widgets.presets       = widget_button( cntl,  value = 'Presets . . .')
    self.widgets.calibrate_energy = widget_button( cntl,  $
                                            value = 'Calibrate energy. . .')
    self.widgets.calibrate_two_theta = widget_button( cntl,  $
                                            value = 'Calibrate 2-theta. . .')

    dspy                         = widget_button( mbar, /menu,              $
                                            font = self.fonts.big_menu,  $
                                            value = 'Display      ')
    self.widgets.display_setup  = widget_button( dspy, $
                                            value = 'Preferences . . .')

    self.widgets.display_jcpds  = widget_button( dspy, $
                                            value = 'JCPDS . . .')
    self.widgets.fit_peaks      = widget_button( dspy, $
                                            value = 'Fit peaks ...')

    help                         = widget_button( mbar, /help,                 $
                                            font = self.fonts.big_menu,  $
                                            value = 'Help')
    self.widgets.help           = widget_button( help, value = 'On window')

    ; Determine a good size to make the widgets in this column
    t = widget_button( base, value='1234567')
    geometry = widget_info(t, /geometry)
    button_size=geometry.scr_xsize
    widget_control, t, /destroy

    acquire              = widget_base(  control_column, /column, $
                                     frame = frame_size, $
                                     /align_center, /base_align_center, $
                                     space=space, xpad=xpad, ypad=ypad)

    w                    = widget_label( acquire, value = 'Acquisition', $
                                    font = self.fonts.label)
    w                   = widget_base(  acquire, /row, space=button_size/3)
    self.widgets.on     = widget_button( w, value = 'On', $
                            font = self.fonts.button, scr_xsize=button_size)
    self.widgets.off    = widget_button( w, value = 'Off', $
                            font = self.fonts.button, scr_xsize=button_size)
    w                   = widget_base(  acquire, /row)
    self.widgets.erase  = widget_button( w, value = 'Erase', $
                            font = self.fonts.button, scr_xsize=button_size)

    status              = widget_base( control_column, /column, $
                                    frame = frame_size, $
                                    /align_center, /base_align_center, $
                                    space=space, xpad=xpad, ypad=ypad)
    w                   = widget_label( status, value = 'Elapsed Time', $
                            font = self.fonts.label)

    row                 = widget_base( status, /row)
    w                   = widget_label( row, value = 'Live', $
                                     scr_xsize = button_size)
    self.widgets.elive  = widget_text( row, xsize = 10)

    row                 = widget_base( status, /row)
    w                   = widget_label( row, value = 'Real', $
                                     scr_xsize=button_size)
    self.widgets.ereal  = widget_text( row, xsize = 10)
    roi                 = widget_base(  control_column, /column, $
                                        frame = frame_size, $
                                        /align_center, /base_align_center, $
                                        space=space, xpad=xpad, ypad=ypad)
    w                   = widget_label(  roi, value = 'ROIs',          $
                                         font = self.fonts.label)
    row                 = widget_base(   roi, /row, space=button_size/3)
    self.widgets.add_roi    = widget_button( row, value = 'Add', $
                                         scr_xsize=button_size)
    self.widgets.del_roi    = widget_button( row, value = 'Delete', $
                                         scr_xsize=button_size)
    row                     = widget_base(   roi, /row)
    self.widgets.clear_roi  = widget_button( row, value = 'Clear All', $
                                         scr_xsize=button_size)
    row                     = widget_base(   roi, /row)
    self.widgets.prev_roi   = widget_button( row, value = ' < ', $
                                         font=self.fonts.big_button)
    self.widgets.label_roi  = widget_text(   row, value = ' ', /edit, xsize=11)
    self.widgets.next_roi   = widget_button( row, value = ' > ', $
                                         font=self.fonts.big_button)

    klm                     = widget_base(  control_column, /column, $
                                        frame = frame_size, $
                                        /align_center, /base_align_center, $
                                        space=space, xpad=xpad, ypad=ypad)
    w                       = widget_label(  klm, value = 'KLM markers',   $
                                        font = self.fonts.label)
    row                     = widget_base(   klm, /row)
    self.widgets.klm_down  = widget_button( row, value = ' < ', $
                                         font=self.fonts.big_button)
    self.widgets.klm_label = widget_text(   row, value = ' ', /edit, xsize=11)
    self.widgets.klm_up    = widget_button( row, value = ' > ', $
                                         font=self.fonts.big_button)

    geometry = widget_info(self.widgets.klm_up, /geometry)
    scr_ysize = geometry.scr_ysize
    display                 = widget_base(   control_column, /column, $
                                         frame = frame_size, $
                                         /align_center, /base_align_center, $
                                         space=space, xpad=xpad, ypad=ypad)
    w                       = widget_label(  display, value = 'Display', $
                                         font = self.fonts.label)
    row                     = widget_base(   display, /row)
    self.widgets.zoom_down = widget_button( row, value = ' < ',     $
                                         font = self.fonts.big_button, $
                                         scr_ysize = scr_ysize)
    w                       = widget_label(  row, value = 'Zoom', $
                                         font = self.fonts.button, $
                                         frame = frame_size,        $
                                         scr_xsize = button_size, /align_center)
    self.widgets.zoom_up   = widget_button( row, value = ' > ',             $
                                         font = self.fonts.big_button, $
                                         scr_ysize = scr_ysize)
    row                     = widget_base(   display, /row)
    self.widgets.shift_down = widget_button( row, value = ' < ',             $
                                         font = self.fonts.big_button, $
                                         scr_ysize = scr_ysize)
    w                       = widget_label(  row, value = 'Shift', $
                                         font = self.fonts.button, $
                                         frame = frame_size,        $
                                         scr_xsize = button_size, /align_center)
    self.widgets.shift_up  = widget_button( row, value = ' > ',            $
                                         font = self.fonts.big_button, $
                                         scr_ysize = scr_ysize)

    w                       = widget_label( display, value = 'Vertical scale', $
                                         font = self.fonts.label)
    self.widgets.lin_log   = widget_droplist( display, $
                                         value = ['Linear','Logarithmic'], $
                                         font = self.fonts.button)
    widget_control, self.widgets.lin_log, set_droplist_select=self.display.vlog

    ; Go down this column and made each widget base the size of the biggest one

    big = 0
    geometry = widget_info(acquire, /geometry)
    big = big > geometry.scr_xsize
    geometry = widget_info(status, /geometry)
    big = big > geometry.scr_xsize
    geometry = widget_info(roi, /geometry)
    big = big > geometry.scr_xsize
    geometry = widget_info(klm, /geometry)
    big = big > geometry.scr_xsize
    geometry = widget_info(display, /geometry)
    big = big > geometry.scr_xsize
    widget_control, acquire, scr_xsize=big
    widget_control, status,  scr_xsize=big
    widget_control, roi,     scr_xsize=big
    widget_control, klm,     scr_xsize=big
    widget_control, display, scr_xsize=big


    self.widgets.plot = widget_draw(data_column, /button_events, $
                             xsize = 100, $   ; The window gets resized below
                             ysize = 100, $
                             frame = frame_size)

    bottom_row   = widget_base( data_column, /row, $
                            space=space, xpad=xpad, ypad=ypad)
    row          = widget_base( bottom_row, /row, frame = frame_size, $
                            space=space, xpad=xpad, ypad=ypad)

    box = widget_base( row, /column, $
                   space=space, xpad=xpad, ypad=ypad)

    ; Determine a good size to make the number widgets
    t = widget_text( box, /edit, xsize=9)
    l = widget_label( box, value = 'Right marker', font = self.fonts.label)
    bsize = widget_info(box, /geometry)
    tsize = widget_info(t, /geometry)
    lsize = widget_info(l, /geometry)
    scr_xsize = bsize.scr_xsize
    scr_ysize = tsize.scr_ysize > lsize.scr_ysize
    widget_control, t, /destroy
    widget_control, l, /destroy

    w                        = widget_label( box, value = ' ')
    self.widgets.horiz_mode = widget_droplist( box, $
                                 value = ['Channel', 'Energy', 'd-spacing'], $
                                 uvalue =['Channel', 'Energy', 'd-spacing'], $
                                 scr_ysize = scr_ysize, $
                                 font = self.fonts.label)
    widget_control, self.widgets.horiz_mode, $
        set_droplist_select=self.display.horiz_mode
    w                        = widget_label( box, value = 'Counts', $
                                 scr_ysize = scr_ysize, $
                                 font = self.fonts.label)

    box             = widget_base( row, /column, $
                                        space=space, xpad=xpad, ypad=ypad)
    w               = widget_label( box, value = 'Cursor', $
                                        font = self.fonts.label)
    self.widgets.cur_pos    = widget_text( box, /edit, scr_xsize=scr_xsize)
    self.widgets.cur_counts = widget_text( box, scr_xsize=scr_xsize)

    box             = widget_base( row, /column, $
                                        space=space, xpad=xpad, ypad=ypad)
    w               = widget_label( box, value = 'Left marker', $
                                   font = self.fonts.label)
    self.widgets.lm_pos    = widget_text( box, /edit, scr_xsize = scr_xsize)
    self.widgets.lm_counts  = widget_text( box, scr_xsize = scr_xsize)

    box             = widget_base( row, /column, $
                                        space=space, xpad=xpad, ypad=ypad)
    w               = widget_label( box, value = 'Right marker', $
                                  font = self.fonts.label)
    self.widgets.rm_pos    = widget_text( box, /edit, scr_xsize = scr_xsize)
    self.widgets.rm_counts  = widget_text( box, scr_xsize = scr_xsize)

    box             = widget_base( row, /column, $
                                        space=space, xpad=xpad, ypad=ypad)
    w               = widget_label( box, value = 'Centroid', $
                                        font = self.fonts.label)
    self.widgets.center_pos = widget_text( box, scr_xsize = scr_xsize)

    box             = widget_base( row, /column, $
                                        space=space, xpad=xpad, ypad=ypad)
    w               = widget_label( box, value = 'FWHM', $
                                  font = self.fonts.label)
    self.widgets.fwhm_pos = widget_text( box, scr_xsize = scr_xsize)

    row             = widget_base( bottom_row, /row, frame=frame_size, $
                                       space=space, xpad=xpad, ypad=ypad)

    box             = widget_base( row, /column, $
                                        space=space, xpad=xpad, ypad=ypad)
    w               = widget_label( box, value = ' ')
    w               = widget_label( box, value = 'Total',  $
                                  scr_ysize = scr_ysize, $
                                  font = self.fonts.label)
    w               = widget_label( box, value = 'Net',   $
                                  scr_ysize = scr_ysize, $
                                  font = self.fonts.label)

    box             = widget_base( row, /column, $
                                        space=space, xpad=xpad, ypad=ypad)
    w               = widget_label( box, value = 'Counts', $
                                font = self.fonts.label)
    self.widgets.total_cts = widget_text( box, scr_xsize=scr_xsize)
    self.widgets.net_cts   = widget_text( box, scr_xsize=scr_xsize)

    box             = widget_base( row, /column, $
                                        space=space, xpad=xpad, ypad=ypad)
    w               = widget_label( box, value = 'CPS', $
                                   font = self.fonts.label)
    self.widgets.total_cps = widget_text( box, scr_xsize=scr_xsize)
    self.widgets.net_cps   = widget_text( box, scr_xsize=scr_xsize)

    self->new_inputs

    ; Set the plot window size to be the same width as the bottom widget row,
    ; and the same height as the control column minus the bottom widget row
    bottom_geom = widget_info(bottom_row, /geometry)
    left_geom = widget_info(control_column, /geometry)
    widget_control, self.widgets.plot, $
                xsize = bottom_geom.xsize $
                         - 2*frame_size - space - xpad, $
                ysize = (left_geom.ysize - bottom_geom.ysize $
                         - 2*frame_size - space - ypad)
end


;*****************************************************************************
function mca_display::init, font_size=font_size, parent=parent

    mca=obj_new('mca')   ; Dummy MCA object
    if (n_elements(parent) ne 0) then spawned=1 else spawned=0

    self.options.version = '4.3.17' ; Version number of program
    self.options.date = 'November 21, 2001' ; Modification date of program
    self.options.warn_overwrite = 1L
                            ; Warn user of attempt to overwrite existing file

    if (spawned) then self.file = parent.file else begin
        self.file.filepath      = '~/'
        self.file.filename      = ''  ; name of saved or read
        self.file.next_filename = 'test.dat'  ; name of next file to save
        self.file.preffile      = 'mca.preferences'
        self.file.jcpds_path    = getenv('JCPDS_PATH')
        self.file.jcpds         = obj_new('JCPDS')
    endelse

    if (spawned) then self.display = parent.display else begin
        self.display.update_time = .5
        self.display.prev_time =    0.D0
        self.display.prev_counts =  0L
        self.display.prev_bgd =     0L
        self.display.new_stats =    0L
        self.display.xsize =      100L ; This gets changed when the window
        self.display.ysize =      100L ;  is created
        self.display.horiz_mode =   0L
        self.display.hmin =         0L
        self.display.hmax =      2048L
        self.display.vmin =         0.
        self.display.vmax =       100.
        self.display.vauto =        1L
        self.display.vlog =         1L
        self.display.lmarker =      100L
        self.display.rmarker =      200L
        self.display.cursor =       300L
        self.display.klm =           26L ; Start with Fe
        self.display.current_roi =    0L
        self.display.psym =         0L
        self.display.psym_choices = [0, 3, 10]
        self.display.psym_names =   ['Solid', 'Dot', 'Histogram']
        self.display.sys_x =        !x
        self.display.sys_y =        !y
        self.display.sys_p =        !p
    endelse

    if (spawned) then self.palette = parent.palette else begin
        self.palette.red =      [  0, 255, 255,   0,   0, 255]
        self.palette.green =    [  0,   0, 255, 255, 255, 255]
        self.palette.blue =     [  0,   0,   0,   0, 255, 255]
    endelse

    ; On X we need to create a window before we can be sure the
    ; information in !d is correct.
    if (!d.name eq 'X') then begin
        window, /free, /pixmap
        wdelete, !d.window
    endif
    if (!d.n_colors gt 256) then begin   ; 24 bit display
        device, decomposed=0
    endif
    self.colors.background =              0L
    self.colors.markers =                 1L
    self.colors.foreground_spectrum =     2L
    self.colors.background_spectrum =     3L
    self.colors.roi =                     4L
    self.colors.labels =                  5L

    if (spawned) then self.print = parent.print else begin
        self.print.xsize =         6.
        self.print.ysize =         4.
        self.print.orientation =    0
        self.print.thick =        1.0
        self.print.charthick =    1.0
        self.print.charsize =     1.0
        self.print.xtitle =     'keV'
        self.print.ytitle =  'Counts'
        self.print.title =        ' '
        self.print.font =           0
    endelse

    self.foreground.nchans = n_elements(self.foreground.data)
    self.background.nchans = n_elements(self.background.data)

    if (spawned) then self.fit = parent.fit else begin
        self.fit.background = mca->background_initialize()
        self.fit.fit = mca->fit_initialize()
        self.fit.ppeaks = ptr_new({MCA_PEAK})
        self.file.fit_results = 'fit_results.txt'
        self.file.fit_spreadsheet = 'fit_spreadsheet.txt'
    endelse

    if (not spawned) then begin
        file = strtrim(getenv('MCA_PREFERENCES'), 2)
        if (file ne '') then self.file.preffile = file
        self->restore_prefs
    endif

    self.fonts = mca_display_get_fonts(font_size=font_size)

    self->create_widgets
    widget_control, self.widgets.base, /realize

    ; Get the actual size of the plot window
    self.display.xsize = !d.x_size
    self.display.ysize = !d.y_size
    window, /free, /pixmap, $
            xsize = self.display.xsize, $
            ysize = self.display.ysize
    self.windows.pixmap = !d.window
    widget_control, self.widgets.plot, get_value = t
    self.windows.plot = t

    self.windows.energy_cal = 10  ; This is ugly

    ; Load the color table
    tvlct, self.palette.red, self.palette.green, self.palette.blue

    ; Clear mouse button variable
    self.windows.mouse_button = 0L

    ; A dummy plot command is needed to establish the data coordinate system
    wset, self.windows.pixmap
    plot, /nodata, self.foreground.data[0 : self.foreground.nchans-1L],      $
               yrange = [self.display.vmin,    self.display.vmax],    $
               xrange = [self.display.hmin-1L, self.display.hmax+1L], $
               xstyle = 5, ystyle = 5, position = [0,0.02,1,1]
    self.display.sys_x = !x
    self.display.sys_y = !y
    self.display.sys_p = !p

    self->update_spectrum, /rescale

    self->lmarker, self.display.lmarker
    self->rmarker, self.display.rmarker
    self->cursor,  self.display.cursor

    if (not spawned) then popup_program_info, $
        font=self.fonts.big_label, $
        title='MCA Display Program Information',  $
        ['IDL MCA Display Program',               $
        'Version ' + self.options.version,      $
        self.options.date,                      $
        ' ',                                     $
        'Mark Rivers',                           $
        'Center for Advanced Radiation Sources', $
        'The University of Chicago']

    widget_control, self.widgets.timer, timer = self.display.update_time
    base = self.widgets.base
    widget_control, base, set_uvalue=self
    xmanager, 'mca_display::init', base, event='mca_display_event', $
                 cleanup = 'mca_display_cleanup', /no_block

    return, 1

end



;*****************************************************************************
pro mca_display__define
    mca = obj_new('mca')
    options = {mca_display_options,   $
        version:        '', $ ; Version number of program
        date:           '', $ ; Modification date of program
        autosave:       0L, $ ; Automatically save file when acq. completes
        autorestart:    0L, $ ; Automatically restart acq. when acq. completes
        warn_overwrite: 0L, $ ; Warn user of attempt to overwrite existing file
        warn_erase:     0L, $ ; Warn user of attempt to erase without prior save
        save_done:      0L, $ ; Flag to keep track of save done before erase
        inform_save:    0L, $ ; Inform user via popup of successful file save
        download_rois:  0L, $ ; Download ROIs to record when reading file
        download_cal:   0L, $ ; Download calibration to record when reading file
        download_data:  0L, $ ; Download MCA data to record when reading file
        debug:          0L  $ ; Debug flag - not presently used
    }

    widgets = {mca_display_widgets,   $
        base:               0L, $
        timer:              0L, $
        rescale:            0L, $
                                $
        foreground_open_det:  0L, $
        foreground_open_file: 0L, $
        background_open_det:  0L, $
        background_open_file: 0L, $
        background_close:     0L, $
        swap_foreground:      0L, $
        file_options:         0L, $
        save_next:            0L, $
        save_as:              0L, $
        new_window:         0L, $
        new_window_tiny:    0L, $
        new_window_small:   0L, $
        new_window_medium:  0L, $
        new_window_large:   0L, $
        print_setup:        0L, $
        print:              0L, $
        exit:               0L, $
                                $
        presets:            0L, $
        calibrate_energy:   0L, $
        calibrate_two_theta: 0L, $
                                $
        display_setup:      0L, $
        display_jcpds:      0L, $
        fit_peaks:          0L, $
        help:               0L, $
                                $
        on:                 0L, $
        off:                0L, $
        erase:              0L, $
                                $
        elive:              0L, $
        ereal:              0L, $
                                $
        add_roi:            0L, $
        del_roi:            0L, $
        clear_roi:          0L, $
        label_roi:          0L, $
        prev_roi:           0L, $
        next_roi:           0L, $
                                $
        klm_label:          0L, $
        klm_down:           0L, $
        klm_up:             0L, $
                                $
        zoom_down:          0L, $
        zoom_up:            0L, $
                                $
        shift_down:         0L, $
        shift_up:           0L, $
                                $
        lin_log:            0L, $
                                $
        horiz_mode:         0L, $
        lm_pos:             0L, $
        lm_counts:          0L, $
        rm_pos:             0L, $
        rm_counts:          0L, $
        cur_pos:            0L, $
        cur_counts:         0L, $
                                $
        total_cts:          0L, $
        net_cts:            0L, $
        total_cps:          0L, $
        net_cps:            0L, $
                                $
        center_pos:         0L, $
        fwhm_pos:           0L, $
                                $
        plot:               0L  $
    }

    file = {mca_display_file, $
        filepath:       '', $
        filename:       '', $ ; name of saved or read
        next_filename:  '', $ ; name of next file to save
        mca_name:       '', $
        jcpds_path:     '', $
        jcpds:          obj_new(), $ ; JCPDS object
        fit_results:    '', $
        fit_spreadsheet: '', $
        preffile:       ''  $
    }

    fit = {mca_display_fit, $
        background: mca->background_initialize(), $
        fit: mca->fit_initialize(), $
        pfit: ptr_new(lonarr(4096)), $
        pbackground: ptr_new(lonarr(4096)), $
        ppeaks: ptr_new() $
    }

    display = { mca_display_display, $
        update_time:   0.0, $
        current_time: 0.D0, $
        current_counts: 0L, $
        current_bgd:    0L, $
        current_acqg:   0L, $
        prev_time:    0.D0, $
        prev_counts:    0L, $
        prev_bgd:       0L, $
        prev_acqg:      0L, $
        new_stats:      0L, $
        xsize:          0L, $
        ysize:          0L, $
        horiz_mode:     0L, $
        hmin:           0L, $
        hmax:           0L, $
        vmin:           0., $
        vmax:           0., $
        vauto:          0L, $
        vlog:           0L, $
        lmarker:        0L, $
        rmarker:        0L, $
        cursor:         0L, $
        klm:            0L, $
        pressure:       0., $
        temperature:    0., $
        current_roi:    0L, $
        psym:           0L, $
        psym_choices: [0, 0, 0], $
        psym_names:   ['', '', ''], $
        sys_x:          !x, $
        sys_y:          !y, $
        sys_p:          !p  $
    }

    windows = {mca_display_windows,  $
        mouse_button: 0L, $
        plot:         0L, $
        energy_cal:   0L, $
        pixmap:       0L  $
    }

    colors = {mca_display_colors,     $
        background: 0L, $
        markers:    0L, $
        foreground_spectrum:   0L, $
        background_spectrum:   0L, $
        roi:        0L, $
        labels:     0L  $
    }

    n_colors = n_tags(colors)
    ; Each column of the palette arrays corresponds to one of the colors above.
    palette = { mca_display_palette,            $
        red:   intarr(n_colors), $
        green: intarr(n_colors), $
        blue:  intarr(n_colors) $
    }

    print = { mca_display_print,   $
        xsize:         0.,   $
        ysize:         0.,   $
        orientation:    0,   $
        thick:         0.,   $
        charthick:     0.,   $
        charsize:      0.,   $
        xtitle:        '',   $
        ytitle:        '',   $
        title:         '',   $
        font:           0    $
    }

    fonts = mca_display_get_fonts()

    nchans = mca->get_nchans()
    roi = mca->get_rois(roi_info)

    foreground = {mca_display_mca, $
        mca:            obj_new(), $
        name:           '', $
        valid:          0L, $
        is_detector:    0L, $
        nchans:         0L, $
        data:           lonarr(nchans), $
        elapsed:        mca->get_elapsed(), $
        nrois:          0L, $
        roi:            replicate(roi, roi_info.MAX_ROIS) $
    }

    obj_destroy, mca

    mca_display = {mca_display, $
        foreground: foreground, $
        background: foreground, $
        options:  options, $
        widgets:  widgets, $
        file:     file,    $
        display:  display, $
        fit:      fit,     $
        windows:  windows, $
        colors:   colors,  $
        palette:  palette, $
        fonts:    fonts,   $
        print:    print    $
    }
end
