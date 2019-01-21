PRO caWidgetDump, pv, widget_id, timer=time
; NAME:
;	caWidgetDump
;
; PURPOSE:
;	Dumps out information about the ezcaIDLWidget data structures
;	Only for debugging use.
;
; CATEGORY:
;	Channel access, IDL widgets
;
; CALLING SEQUENCE:
;	caWidgetDump
;
; COMMON BLOCKS:
;	caWidgetCommon
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers, July, 1995


common caWidgetCommon, mon_names, mon_pointers, mon_wids, wevent, wbase, poll_time

print, 'mon_names = ' 
for i=0, n_elements(mon_names)-1 do print, '       ', mon_names(i)
print, 'mon_pointers = '
for i=0, n_elements(mon_pointers)-1 do print, '       ', mon_pointers(i)
print, 'mon_wids = '
for i=0, n_elements(mon_wids)-1 do print, '       ', mon_wids(i)
print, 'poll_time = ', poll_time
end

FUNCTION caWidgetClearMonitor, pv, widget_id
;+
; NAME:
;	caWidgetClearMonitor
;
; PURPOSE:
;	This function clears a channel access IDL widget monitor which was 
;       set with caWidgetSetMonitor
;
; CATEGORY:
;	EPICS channel access; IDL widgets
;
; CALLING SEQUENCE:
;	status = caWidgetClearMonitor(pv, widget_id)
;
; INPUTS:
;	pv:     The name of the process variable associated with this widget
;
;       widget_id:  The widget ID of the widget which was called on monitors.
;
; OUTPUTS:
;   The function return value of caWidgetClearMonitor is a status value.  The
;   status is 0 if the routine was successful (i.e. the process variable exists)
;   and non-zero if the routine failed.
;
; COMMON BLOCKS:
;	caWidgetCommon:
;
; SIDE EFFECTS:
;	This routine will stop channel access monitoring of this process
;       variable if there are no other widgets monitoring this process variable.
;	this entry.
;
; PROCEDURE:
;	If this is the last widget monitoring this process variable then
;       caClearMonitor is called.
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers, July 20, 1995
;-

common caWidgetCommon

; If mon_names doesn't exist then caWidgetAddMonitor has never been called
if (n_elements(mon_names) eq 0) then return, 0

pointer = where((pv eq mon_names), count)
if (count le 0) then return, 0	; This wasn't a pv we were monitoring

; Compute the indices of all elements in these arrays which should
;  be deleted
bad = where((mon_pointers eq pointer) and (mon_wids eq widget_id),count)
if (count le 0) then return, 0	; This wasn't a widget we were monitoring

; Clear the entries in mon_pointers and mon_names
mon_pointers(bad) = -1
mon_wids(bad) = -1

; Now check to see if there are any other widgets monitoring this pv.
t = where((mon_pointers eq pointer), count)
if (count le 0) then begin 
   ; No other widgets are monitoring this pv so clear the entry in 
   ; mon_names and stop the channel access monitoring of this pv.
   t = caClearMonitor(pv)
   mon_names(pointer) = ""
endif
end


FUNCTION caWidgetSetMonitor, pv, widget_id, time=time
;+
; NAME:
;	caWidgetSetMonitor
;
; PURPOSE:
;	This function establishes a channel access monitor on a process
;       variable.  It causes a widget event for widget_id to be generated
;       whenever a monitor arrives for that process variable.
;
; CATEGORY:
;	EPICS channel access; IDL widgets
;
; CALLING SEQUENCE:
;	status = caWidgetSetMonitor(pv, widget_id, time=time)
;
; INPUTS:
;	pv:     The name of the process variable to be monitored.
;
;       widget_id:  The widget ID of the widget to be called on monitors.
;
; KEYWORD PARAMETERS:
;	time:	The time interval between polling to check for new monitors.
;               The default is 0.1 seconds.
;
; OUTPUTS:
;       The function return value of caWidgetSetMonitor is a status value.
;       The status is 0 if the routine was successful (i.e. the process 
;       variable exists) and non-zero if the routine failed.
;
; COMMON BLOCKS:
;	caWidgetCommon:
;
; SIDE EFFECTS:
;	This routine will start channel access monitoring of this process
;       variable if there are not already other widgets monitoring this 
;       process variable.
;
; PROCEDURE:
;       If this is the first time caWidgetSetMonitor has been called 
;       then it creates a dummy (iconified) widget which runs a timer routine. 
;       The timer routine periodically calls caCheckMonitor(pv) to determine 
;       whether a channel access monitor has arrived for "pv". If a monitor 
;       has occurred then an event will be sent to the widget whose ID is 
;       specified by "widget_id". 
;       The event structure is as follows:
;       event = 
;         { id         ; The widget ID which was passed to caWidgetSetMonitor
;           top:       ; The top level widget in this hierarchy
;           handler:   ; The widget handler routine
;           name:      ; The name of the process variable for which a monitor 
;                      ; has occurred.
;         }
;       When the event is sent, the event handler routine for the specified 
;       widget will be called. Generally this routine look at the event.id 
;       field to determine that this is a monitor event (rather than a mouse 
;       event).  If the same event handler can receive monitor events from 
;       more than one process variable, (because caWidgetSetMonitor was 
;       called for several process variables) the event handler will then 
;       look at the event.name field to determine which process variable 
;       generated the monitor event.
;
;       Typically the widget_id which is passed to caWidgetSetMonitor
;       should be the id of a base widget. Base widgets cannot generate 
;       events due to mouse clicks, etc. so the widget event handler routine 
;       can distinguish monitor events from mouse events by looking at the 
;       widget.id field. This is the same concept which is described in the 
;       IDL documentation for timer events, e.g.
;           widget_control, wid, timer=1.0
;
;       caWidgetSetMonitor can be called for many different process variable 
;       names and widget_ids. The widgets do not need to belong to the same 
;       widget hierarchy. Multiple widgets can monitor the same process 
;       variable, and the same widget can be used to monitor several process 
;       variables.  Internally  caWidgetSetMonitor maintains a list of all 
;       monitored process variables, and which widget_id(s) are to receive 
;       events from each process variable.
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers, July 20, 1995
;-

common caWidgetCommon

	if (n_elements(time) ne 0) then poll_time=time

; If this is the first time we were called then set things up
	if (n_elements(mon_names) eq 0) then begin
	   if (n_elements(poll_time) eq 0) then poll_time = 0.1
	   mon_names = pv
	   mon_pointers=0
	   mon_wids = widget_id
	   wevent = { $
			id:	0L, $
			top:	0L, $
			handler: 0L, $
			name:	"" $
		   }
    	   wbase = -1L 
	endif else begin
	   ; Look and see if the pv is already being monitored
	   index = where(pv eq mon_names)
	   index = index(0)
	   if (index lt 0) then begin  ; This is a new name
		; See if there are any unused array elements in mon_names
		index = where(mon_names eq "")
		index = index(0)
		if (index ge 0) then begin	; Yes, put new name there
		   mon_names(index) = pv
		endif else begin
		   index = n_elements(mon_names) ; No, append new name to end
		   mon_names = [mon_names, pv]
		endelse
	   endif
	   t = where((mon_pointers eq index) and (mon_wids eq widget_id), found)
	   if (not found) then begin
		; See if there are any unused array elements in mon_pointers
		free = where(mon_pointers eq -1)
		free = free(0)
		if (free ge 0) then begin	; Yes, put new pointer there
		   mon_pointers(free) = index
		   mon_wids(free) = widget_id
		endif else begin		; No, append new entries to end
		   mon_pointers = [mon_pointers, index]
		   mon_wids = [mon_wids, widget_id]
		endelse
	   endif
	endelse
	t = casetmonitor(pv)
	; Is the wbase widget valid? If not, start it
	if (not widget_info(wbase, /valid_id)) then begin
	   wbase = widget_base(title='CaWidget')
	   widget_control, wbase, /realize, /icon, event_pro='cawidget_timer'
	   widget_control, wbase, timer=poll_time
	endif
end


PRO cawidget_timer, event
; NAME:
;	caWidgetTimer
;
; PURPOSE:
;	This routine is called as a timer routine. It checks for monitors on
;       process variables which have been established with caWidgetSetMonitor()
;       and sends the appropriate widget events.
;
; CATEGORY:
;	Channel access, IDL widgets
;
; CALLING SEQUENCE:
;	Not called directly. xmanager calls this routine on timer events.
;
; COMMON BLOCKS:
;	caWidgetCommon
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers, July, 1995
common caWidgetCommon

; This routine is called periodically to check for monitors
; Look through all the pvs we are monitoring
	for i=0, n_elements(mon_names)-1 do begin
	   name = mon_names(i)
	   if (name eq "") then goto, skip
	   if (caCheckMonitor(name) eq 0) then goto, skip
	   ; Look for widgets waiting for this monitor
	   index = where((mon_pointers eq i), count)
	   for j=0, count-1 do begin
		wid = mon_wids(index(j))
		if (widget_info(wid, /valid)) then begin
		   wevent.id = wid
		   wevent.name = name
		   widget_control, wid, send_event=wevent
		endif else begin
		   t = caWidgetClearMonitor(name, wid)
		endelse
	   endfor
	   skip:
	endfor
	widget_control, event.id, timer=poll_time
end



PRO caWidgetAdjust_event, event
;
; This is the event handler for caWidgetAdjust.  See the documentation of that
; routine for information.

; Fetch the variables stored in the uvalue of the base widget.
; They are stored there, rather than in common blocks, so that
; multiple instances of these widgets can coexist

widget_control, event.top, get_uvalue=t
widget_ids = t.widget_ids
pv = t.pv

case event.id of

   widget_ids.monitor: begin
        status = caget(pv, val, max=1)
	case t.pv_type of
	  0: widget_control, widget_ids.string, set_value=val
	  3: begin
		for i=0, t.n_buttons-1 do begin
		   widget_control, widget_ids.buttons(i), set_button=0
		endfor
		widget_control, widget_ids.buttons(val), set_button=1
	     end    
	  else: widget_control, widget_ids.slider, set_value=val
	endcase
   end

   widget_ids.slider: begin
	widget_control, widget_ids.slider, get_value=value
	t = caput(pv, value)
   end
	
   widget_ids.string: begin
	widget_control, widget_ids.string, get_value=value
	t = caput(pv, value)
   end
	
   widget_ids.exit: begin
	t=cawidgetclearmonitor(pv, widget_ids.monitor)
	widget_control, event.top, /destroy
   end

   else: begin
    ; Was one of the buttons in a menu pressed?
	button = where(event.id eq widget_ids.buttons)
	button = button(0)
	if (button ne -1) then begin
	   t = caput(pv, button)
	endif else begin
	   message, "Unknown button pressed", /continue
	endelse
   end

endcase
end


PRO caWidgetAdjust, input_pv, font=font, min=min, max=max, label=label, $
			group=group
;+
; NAME:
;	caWidgetAdjust
;
; PURPOSE:
;       This is a general purpose routine for adjusting and monitoring a process
;       variable. It creates widget which is appropriate for the data type 
;       of "pv", i.e. a mutually exclusive menu for DBF_ENUM, a text entry 
;       widget for DBF_STRING, and an editable slider widget for any numeric 
;       data type. This routine can be called from the event handler of larger 
;       applications when all that needs to be done is adjust the value of a 
;       process variable.
;
; CATEGORY:
;	EPICS channel access; IDL widgets
;
; CALLING SEQUENCE:
;	caWidgetAdjust, pv, font=font, min=min, max=max, label=label, $
;                       group=group
;
; INPUTS:
;	pv:     The name of the process variable to be adjusted
;
; KEYWORD PARAMETERS:
;	font:   This keyword can be used to specify a font to use.
;
;	min:    This keyword can be used to specify a lower limit for the
;               slider widget when adjusting numeric process variables.
;
;	min:    This keyword can be used to specify an upper limit for the
;               slider widget when adjusting numeric process variables.
;
;       label:  This keyword can be used to put a descriptive label at the 
;               top of the widget.
;
;       group:  This keyword can be used to set the id of the parent widget. 
;               If the widget specified by "group" is deleted, then the 
;               widget created by CaWidgetAdjust will also be deleted.
;
; SIDE EFFECTS:
;	This routine will start channel access monitoring of this process
;       variable if there are not already other widgets monitoring this 
;       process variable.
;
; PROCEDURE:
;       Builds a top-level widget to adjust this process variable.
;
; MODIFICATION HISTORY:
; 	Written by:	Mark Rivers, July 20, 1995
;-

widget_ids = {caWidgetAdjust_wids,  $
	value:		0L, $
        monitor:	0L, $
        label:		0L, $
        name:		0L, $
        string:		0L, $
        buttons:	lonarr(20), $
        slider:		0L, $
        exit:		0L $
}

pv = input_pv
n_buttons=0
base = widget_base(title="Channel Access Adjust")
if (n_elements(font) eq 0) then font = ''
widget_control, base, default_font=font
widget_ids.monitor = base
col1 = widget_base(base, column=1)
if (n_elements(label) eq 0) then label=' '
widget_ids.label = widget_label(col1, value=label)
widget_ids.name = widget_label(col1, value='PV = '+pv)

; Here we switch according to the data type of the field
status = caGetCountAndType(pv, count, type)
pv_type = type(0)
if pv_type eq 0 then begin
   ; This is a string variable
   status = caget(pv, value)
   widget_ids.string = widget_text(col1, value=value, /edit)
endif else if pv_type eq 3 then begin
   ; This is an enum field
   status = caGetEnumStrings(pv, value)
   xmenu, button=button_ids, title="Choice", /exclusive, /no_release, $
          value, col1 
   ; Now find out which enum value is presently selected and select that button
   status = caGet(pv, value)
   widget_control, button_ids(value), set_button=1
   widget_ids.buttons = button_ids 
   n_buttons = n_elements(button_ids)
endif else begin
   ; This is a numeric field, use a slider
   status = caGet(pv, value, max=1)
   if (n_elements(min) eq 0) then min=-10*abs(value)
   if (n_elements(max) eq 0) then max=10*abs(value) > 10
   widget_ids.slider = cw_fslider(col1, value=value, /drag, /edit, $
                                  min=min, max=max)
endelse
widget_ids.exit = widget_button(col1, value="Exit")
t = { $
	widget_ids:	widget_ids, $
	pv:		pv, $
        pv_type:	pv_type, $
	n_buttons:	n_buttons $
}
widget_control, base, set_uvalue=t
widget_control, base, /realize
t=caWidgetSetMonitor(pv, widget_ids.monitor)
xmanager, "caWidgetAdjust", base, group_leader=group
end
