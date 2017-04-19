;+
; NAME:
; BGDIRTREE_WIDGET
;
; PURPOSE:
; A compound widget object that provides a means for navigating directories and
; selecting files using a tree view. Requires the following external routines:
; WHEREVER
; BG_GET_DRIVES
; PROGRAMROOTDIR
;
; CATEGORY:
; Widgets.
;
; CALLING SEQUENCE:
; Result = BGDirTree_Widget(Parent)
;
; INPUTS:
; Parent: The ID of the parent widget
;
; KEYWORD PARAMETERS:
; Event_func -- a user defined event handler function for the widget
; Event_pro -- a user defined event handler procedure for the widget
; Files_only -- set this keyword to only allow files to be selected
; Filter -- this keyword defines which files are shown in the tree. '*.txt' shows only text files '*.*' shows all files
; Frame -- the widget frame thickness
; Multiple -- set this keyword to allow multiple selections
; Path -- set this keyword to a string path to select on startup
; Sensitive -- set this keyword to 0 to desensitize the widget on startup
; Uname -- a user name for the widget
; Uvalue -- a user value for the compount widget
; XSize -- the xsize in pixels for the widget
; YSize -- the ysize in pixels for the widget
; Rooted -- disable the drive list; only display files and folders below the root folder.
;
; OUTPUTS:
; The widget returns event structures with the name BGDirTree_Event. The
; structure contains the following fields:
;
; ID: the widget ID
; TOP: the top widget ID
;  HANDLER: the widget handler ID
; FILE: the selected file name
; PATH: the selected file path
; NODE_ID:  the widget ID of the selected node
; N_SELECTED: the number of nodes that were selected. If this is greater than 1,
;         then use the get_value keyword in widget_info to return a structure
;         containing the selected items.
; OBJECT: a reference to the widget object.
;
;
; Using the get_value keyword in Widget_control returns a structure with the following fields:
; NAMES  a string array of pathnames for the selected object(s) if no objects are selected, then SELECTED=''
; NODE_ID:  an arrays of widget IDs for the selected objects
; N_SELECTED: the number of selected objects.
; OBJECT: a reference to the widget object.
;
; OBJECT PROCEDURE METHODS:
; Select,path -- select a file or folder. expand any parent folders.
; Set_Value,value -- sets the current value of the widget. Same result as calling widget_control
;            with the set_value keyword
;
; OBJECT FUNCTION METHODS:
; Is_File(files) -- checks to see if the string(s) is a filename.
;             Assumes that directories have the trailing separator.
; GetID() -- returns the ID of the widget object
; Get_Value() -- returns the current value of the widget. Same result as calling widget_control
;           with the get_value keyword
;
; EXAMPLE:
;
; See the example procedure at the bottom of the file.
;
;
; MODIFICATION HISTORY:
;
; Written by Brad Gom, 15 Oct 2005
; Oct 26  BGG Changed to an object widget
; Jun 11 2008 (BGG) - reverted to using findfile since file_search is too slow.
; May 31 2013 (BGG) - added setProperty method to change the filter.
; Jul 30 2013 (BGG) - added wrappers for file_search as workaround for slow searches on network shares with many files.
; Jun 15 2015 (BGG) - fixed bug in VM/RT mode. Still very slow.
; Apr 19 2017 (BGG) - merged in edits from heldermarchetto to allow use in VM mode and Linux
;
; *Note there is a bug in IDL 6.2 where de-selecting items in a tree does not always generate events. This
;   causes problems when selecting and deselecting ranges using ctrl or shift and the arrows or mouse.
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2015, by Brad Gom. All rights reserved.                                   ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * The author's name may not be used to endorse or promote products derived          ;
;        from this software without specific prior written permission.                     ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY BRAD GOM ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,  ;
;  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS    ;
;  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL BRAD GOM BE LIABLE FOR ANY   ;
;  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,  ;
;  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR  ;
;  PROFITS; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND    ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
;-

function listfiles,path,pattern,count=count,_extra=e    ;wrapper to avoid the file_search() slowness
  if n_elements(pattern) eq 0 then pattern='*'
  case strupcase(!version.os_family) of
    'WINDOWS':begin
      if STRPOS(path,path_sep(),/reverse_search) eq strlen(path)-1 then path=STRMID(path, 0,strlen(path)-1)   ;get rid of trailing slash. This happens for drive roots, but seems to always happen for other folders?
      spawn, 'dir "'+path+path_Sep()+pattern+'" /b /a-D /ON',result,/hide   ;double quotes required for folders with spaces
      count = (result[0] eq '') ? 0 : n_elements(result)
      return,file_dirname(path+path_Sep()+pattern,/mark)+result
    end
    'UNIX':begin
      if STRPOS(path,path_sep(),/reverse_search) eq strlen(path)-1 then path=STRMID(path, 0,strlen(path)-1)   ;get rid of trailing slash. This happens for drive roots, but seems to always happen for other folders?
      spawn, "find "+path+" -maxdepth 1 -type f -name '"+pattern+"'",result
      count = (result[0] eq '') ? 0 : n_elements(result)
      return,result
    end
  endcase
end

function listdirs,path,count=count,_extra=e   ;wrapper to avoid the file_search() slowness
  ;Path in this case is the directory to search in. the search filter is *
  case strupcase(!version.os_family) of
    'WINDOWS':begin
      spawn, 'dir "'+path+'" /b /aD /ON',result,/hide
      count = (result[0] eq '') ? 0 : n_elements(result)
      return, file_dirname(path+'*',/mark)+result+'\'
    end
    'UNIX':begin
      if STRPOS(path,path_sep(),/reverse_search) ne strlen(path)-1 then path+=path_sep()
      spawn, 'for i in $(ls -d '+path+'*/); do echo ${i%%/}; done',result
      count = (result[0] eq '') ? 0 : n_elements(result)
      return, result+'/'
    end
  endcase
end

function BGDirTree::add,id,filter,full=full
  COMPILE_OPT HIDDEN
  ;***can't use expanded when in self->select for a file that isn't shown yet..
  ;if the full keyword is set, add all the contents, not just the first child for speed..

  ;if check_only is set, and the current node isn't expanded, then only the first child file or folder is added.
  if not widget_info(id,/valid) then return,0

  widget_control,id,get_uvalue=uval,/no_copy

  if uval.type ne 'folder' then begin
    widget_control,id,set_uvalue=uval,/no_copy
    return,0
  endif
  if uval.path eq '' then stop

  ;get a list of the directory content, and count the subfolders and files
  foldercount=0
  filecount=0

  if self.debug then begin
    print,'finding folders in '+uval.path
    ;    tic
  endif
  dirs=listdirs(uval.path,count=foldercount)
  if self.debug then begin
    ;    toc
    print,'found '+strtrim(foldercount,2)+' folders: ',dirs
  endif

  if self.debug then begin
    print,'finding files in '+uval.path
    ;    tic
  endif
  files=listfiles(uval.path,filter,count=filecount)
  if self.debug then begin
    ;    toc
    print,'found '+strtrim(filecount,2)+' files: ',files
  endif

  if filecount gt 0 then begin
    files=files[sort(files)]
  endif

  if not widget_info(id,/tree_expanded) and not keyword_set(full) then begin
    ;if we found files/folders, and the node is empty, then add the first one
    child=widget_info(id,/child)
    if child eq 0 then begin
      if foldercount gt 0 then begin
        parts=strsplit(dirs[0],path_sep(),/extract,count=count)
        if count eq 0 then stop
        name=parts[count-1]
        leaf = WIDGET_TREE(id, VALUE=name, /folder, UVALUE={Method:"MainEvents", Object:self, path:dirs[0], type:'folder'}) ;stash:uval.stash,
        widget_control,id,set_uvalue=uval,/no_copy
        return,1
      endif
      if filecount gt 0 then begin
        parts=strsplit(files[0],path_sep(),/extract,count=count)
        name=parts[count-1]
        leaf = WIDGET_TREE(id, VALUE=name, UVALUE={Method:"MainEvents", Object:self, path:files[0], type:'file'}) ;stash:uval.stash,
        widget_control,id,set_uvalue=uval,/no_copy
        return,1
      endif
    endif else begin
      ;if the node isn't empty but we found no files/folders, then delete all the children.
      if foldercount eq 0 and filecount eq 0 then begin
        while child ne 0 do begin
          widget_control,child,/destroy
          child=widget_info(id,/child)
        endwhile
      endif
      widget_control,id,set_uvalue=uval,/no_copy
      return,0  ;skip adding the rest.
    endelse
  endif

  ;add the leaf if it doesn't exist in the tree. Delete any leaf that isn't in the list.

  ;first, get a list of all the existing folders
  ids=widget_info(id,/child)

  treefoldercount=0
  treefilecount=0
  if ids ne 0 then begin  ;there are already children in this branch.
    widget_control,ids,get_uval=child_uval
    sibling=widget_info(ids,/sibling)
    while sibling ne 0 do begin
      ids=[ids,sibling]
      widget_control,sibling,get_uval=sibling_uval
      child_uval=[child_uval,sibling_uval]
      sibling=widget_info(sibling,/sibling)
    endwhile

    ;child_uval is now an array of all the children uvalues
    dirinds=where(child_uval.type eq 'folder',treefoldercount)
    fileinds=where(child_uval.type eq 'file',treefilecount)
  endif

  ;if there are folders in the tree
  if treefoldercount gt 0 then begin
    dirids=ids[dirinds] ;the ids of the folders.

    ;delete any existing folders that aren't in the list
    bad_inds = wherever(  (child_uval[dirinds]).path, dirs, c, /not_eq)
    for i=0,c-1 do begin
      widget_control,ids[bad_inds[i]],/dest
    endfor

    if foldercount ne 0 then begin
      ;make a list of any directories that don't yet exist
      new_inds=wherever(dirs,(child_uval[dirinds]).path, foldercount,/not_eq)
      if foldercount ne 0 then dirs=dirs[new_inds]  ;these directories must be added
    endif

  endif ;there are no existing folders

　
  ;if there are files in the tree
  if treefilecount gt 0 then begin
    fileids=ids[fileinds] ;the ids of the files.

    ;delete any existing files that aren't in the list
    bad_inds = wherever((child_uval[fileinds]).path, files, c, /not_eq)
    for i=0,c-1 do begin
      widget_control,ids[bad_inds[i]],/dest
    endfor

    if filecount ne 0 then begin
      ;make a list of any files that don't yet exist
      new_inds=wherever(files,(child_uval[fileinds]).path, filecount,/not_eq)
      if filecount ne 0 then files=files[new_inds]
    endif

  endif;there are no existing files

  for i=0,foldercount-1 do begin  ;add subfolders if required.
    parts=strsplit(dirs[i],path_sep(),/extract,count=count)
    name=parts[count-1]
    leaf = WIDGET_TREE(id, VALUE=name, /folder, UVALUE={Method:"MainEvents", Object:self, path:dirs[i], type:'folder'}) ;stash:uval.stash,
  endfor

  for i=0,filecount-1 do begin  ;add subfolders if required.
    parts=strsplit(files[i],path_sep(),/extract,count=count)
    name=parts[count-1]
    leaf = WIDGET_TREE(id, VALUE=name, UVALUE={Method:"MainEvents", Object:self, path:files[i], type:'file'}) ;stash:uval.stash,
  endfor

  widget_control,id,set_uvalue=uval,/no_copy
  return,foldercount+filecount
end

pro BGDirTree::Cleanup
  COMPILE_OPT HIDDEN
  ptr_free,self.uvalue
end

function BGDirTree::ContextEvents, ev
  ;The event handler method for the context menu
  COMPILE_OPT HIDDEN

  widget_control, ev.id, GET_UVALUE=uval

  retfile=''
  retpath=''

  case uval.type of
    'select all':begin
      ;if a folder was selected, select all the children files
      ;if a file was selected, select all sibling files
      ids=widget_info(self.tree_ID,/tree_select)
      id=ids[0]
      if not widget_info(id,/valid) then return,-1
      widget_control,self.tlb,/hourglass
      widget_control,id,get_uvalue=u
      case u.type of
        'folder':begin
          widget_control,id,set_tree_select=0
          retpath=file_dirname(u.path,/mark_dir)
          child=widget_info(id,/child)  ;the first child
          while child ne 0 do begin ;there are children
            widget_control,child,get_uvalue=child_uval
            if child_uval.type eq 'file' then begin
              widget_control,child,/set_tree_select
              if retfile eq '' then begin ;return the first filename in the event
                retfile=file_basename(child_uval.path)
              endif
            endif
            child=widget_info(child,/sibling)
          endwhile
        end
        'file':begin
          retpath=file_dirname(u.path,/mark_dir)
          retfile=file_basename(u.path)
          ;get a list of all the other files in the folder
          parent=widget_info(id,/parent)
          child=widget_info(parent,/child)  ;the first child
          while child ne 0 do begin ;there are children
            widget_control,child,get_uvalue=child_uval
            if child_uval.type eq 'file' then widget_control,child,/set_tree_select
            child=widget_info(child,/sibling)
          endwhile
        end
        else:
      endcase
      widget_control,self.tlb,hourglass=0
    end
    'refresh':begin
      ids=widget_info(self.tree_ID,/tree_select)
      id=ids[0]
      if not widget_info(id,/valid) then return,-1
      widget_control,self.tlb,/hourglass
      ;      widget_control,self.tree_ID,map=0
      widget_control,self.tree_id,update=0
      widget_control,id,get_uvalue=u
      case u.type of
        'folder':begin
          self->empty,id
          self->refresh,id,self.filter
        end
        'file':begin
          dir=widget_info(id,/parent)
          self->empty,dir
          self->refresh,dir,self.filter
          self->select,u.path
        end
        else:
      endcase
      ;      widget_control,self.tree_ID,map=1
      widget_control,self.tree_id,update=1
      widget_control,self.tlb,hourglass=0
      return,-1 ;don't return an event on refresh
    end
    'rename': begin
      ;get the first selected file
      ids=widget_info(self.tree_ID,/tree_select)
      id=ids[0]
      if not widget_info(id,/valid) then return,-1
      widget_control,id,get_uvalue=u
      name=u.path
      if u.type eq 'file' then begin
        b=widget_base(group_leader=self.tlb,/col,/modal,/base_align_center,title='File rename')
        t=widget_label(b,value='Enter new name for '+name+':')
        basename=file_basename(name)
        dir=file_dirname(name,/mark)

        input=fsc_inputfield(b,/stringvalue,title='New name:',xsize=strlen(basename)+5,value=basename)
        x=widget_base(b,/row)
        y=widget_button(x,value='Canel',uvalue='cancel')
        y=widget_button(x,value='Accept',uvalue='accept')

        widget_control,b,/real
        event=widget_event(b,bad_id=bad)
        if bad eq 0 then begin
          widget_control,event.id,get_uvalue=uval
          case uval of
            'accept' : begin
              newname = strtrim(input->get_value(),2)
              if newname eq '' then return,-1
              catch,err
              if err ne 0 then begin
                result=dialog_message(['Failed to rename file!',!ERROR_STATE.msg],/err,title='Rename error',/center,dialog_parent=self.tlb)
                catch,/cancel
                if widget_info(b,/valid) then widget_control,b,/dest
                return,-1
              endif

              file_move,name,dir+newname,/allow_same
              self->refresh,widget_info(id,/parent),self.filter
            end
            'cancel':begin
            end
            else:
          endcase
        endif
        if widget_info(b,/valid) then widget_control,b,/dest
      endif
      return,-1 ;don't return an event on rename
    end
    'delete':begin
      ;get the selected files
      ids=widget_info(self.tree_ID,/tree_select)
      id=ids[0]
      if not widget_info(id,/valid) then return,-1
      n=n_Elements(ids)
      names=strarr(n)
      isfile=intarr(n)
      for i=0,n-1 do begin
        widget_control,ids[i],get_uvalue=u
        names[i]=u.path
        if u.type eq 'file' then isfile[i]=1
      endfor

      inds=where(isfile,count)
      if count gt 0 then begin
        names=names[inds]
        ids=ids[inds]

        if count le 10 then begin
          prompt=['Delete the following '+strtrim(count,2)+' file(s)?',names]
        endif else begin
          prompt=['Delete the following '+strtrim(count,2)+' file(s)?',names[0:9],'...',names[count-1]]
        endelse

        result=dialog_message(prompt,/question,dialog_parent=self.tlb,$
          /center,title='WARNING! Confirm File Delete')
        if result eq 'Yes' then begin
          ;get the selected file
          file_delete,names
          for i=0,count-1 do begin
            if widget_info(ids[i],/valid) then widget_control,ids[i],/destroy
          endfor
        endif
      endif
      return,-1 ;don't return an event on delete
    end
    else:
  endcase

  sel = widget_info(self.tree_id,/tree_select)
  nsel = n_elements(sel)

  ret = { BGDirTree_EVENT, ID:self.tlb, TOP:ev.top, HANDLER:ev.top, $
    FILE:retfile, path:retpath, node_id:ev.id, n_selected:nsel, object:self }   ;Create an event.

  RETURN, ret
end

pro BGDirTree::empty,id
  COMPILE_OPT HIDDEN
  ;deletes the contents of a given node.
  if not widget_info(id,/valid) then return

  widget_control,id,get_uvalue=uval
  if uval.type ne 'folder' and uval.type ne 'root' then return

  child=widget_info(id,/child)
  while child ne 0 do begin
    sibling=widget_info(child,/sibling)
    widget_control,child,/destroy
    child=sibling
  endwhile
end

FUNCTION BGDirTree_Event_Handler, event
  ; The main event handler for the compound widget. It reacts
  ; to "messages" in the UValue of the widget.
  ; The message indicates which object method to call. A message
  ; consists of an object method and the self object reference.
  COMPILE_OPT HIDDEN
  Widget_Control, event.ID, Get_UValue=theMessage
  result = Call_Method(theMessage.method, theMessage.object, event)

  RETURN, result
END

function BGDirTree::find,path ;returns ids of branches matching the path(s)
  COMPILE_OPT HIDDEN
  if path[0] eq '' then return,0

  ;start at the root, then look for each folder in turn. If the folder doesn't exist yet,
  ;then fill it.
  this_node=0L

  ;first, select the first drive in the tree
  folderID=widget_info(self.tree_id,/child)
  ids=[0L]  ;a list of ids for the matching items
  if folderID eq 0 then return,0

  for i=0,n_elements(path)-1 do begin
    ;start at the root, then look for each folder in turn. If the folder doesn't exist yet,
    ;then fill it.

    ;should probaly use a string test instead of looking for the file
    ;if file_test(path[i],/dir) then folder=1 else folder=0

    parts=strsplit(path[i],path_sep(),/extract,count=count)
    this_node=0L
    ;first, select the first drive in the tree
    folderID=widget_info(self.tree_id,/child)
    if folderID ne 0 then begin
      for j=0,count-1 do begin  ;step through the path parts, and see if any siblings match
        if j eq count-1 then select=1 else select=0   ;only select the final item in the path.

        while folderID ne 0 do begin  ;search the siblings and find a match until no more siblings (folderID=0)
          widget_control,folderID,get_value=name

          if strlowcase(name) eq strlowcase(parts[j]) then begin
            if select then ids=[ids,folderID] ;found a match, add it to the list
            break ; go to next part
          endif
          ;go to the next sibling folder
          folderID=widget_info(folderID,/sibling)
        endwhile
        ;either a match was found for the part, or we ran out of siblings
        if folderID eq 0 then break ;no match was found. go to next path
        ;go down a level
        folderID=widget_info(folderID,/child)
        ;check the next part
      endfor
    endif
  endfor
  if n_elements(ids) eq 1 then return,0 ;nothing found
  return,ids[1:*]
end

FUNCTION BGDirTree::GetID
  COMPILE_OPT HIDDEN
  ; This method returns the ID of the top-level base of the compound widget.
  RETURN, self.tlb
END

function BGDirTree::get_value
  COMPILE_OPT HIDDEN

  sel = widget_info(self.tree_id,/tree_select)
  nsel = n_elements(sel)

  if sel[0] ne -1 then begin
    paths=strarr(nsel)
    for i=0,nsel-1 do begin
      widget_control,sel[i],get_value=name,get_uvalue=uval
      paths[i]=uval.path
    endfor
  endif else paths=''

  if self.files_only then begin
    inds=where(self->is_file(paths),nsel)
    if nsel eq 0 then begin
      paths=''
      sel=0l
    endif else begin
      paths=paths[inds]
      sel=sel[inds]
    endelse
  endif

  val={ object:self, paths:paths, node_id:sel, n_selected:nsel }  ;Create an event.
  return,val
end

function BGDirTree_get_value,id
  COMPILE_OPT HIDDEN
  ;an interface to the get_value method for use by external programs calling widget_control.
  stash=widget_info(id,/child)
  widget_control,stash,get_uvalue=uval
  obj=uval.object

  return,obj->get_value()
end

function BGDirTree::is_file,files
  COMPILE_OPT HIDDEN
  ;checks to see if the string is a filename. Assumes that directories have the trailing separator.
  n=  n_elements(files)
  result=intarr(n)
  lastslash=strpos(files,path_sep(),/reverse_search)
  return, strpos(files,path_sep(),/reverse_search) ne strlen(files)-1
end

pro BGDirTree_Kill_Notify,id
  COMPILE_OPT HIDDEN
  widget_control,id,get_uvalue=uval
  obj_destroy,uval.object
end

function BGDirTree::MainEvents, ev
  COMPILE_OPT HIDDEN
  ;The main event handler method for the compount widget

  ;trap the context events first
  if (tag_names(ev,/structure_name) eq 'WIDGET_CONTEXT') then begin
    contextBase = WIDGET_INFO(widget_info(ev.id,/parent), FIND_BY_UNAME = 'contextMenu')
    ;help,ev,/str
    WIDGET_DISPLAYCONTEXTMENU, ev.ID, ev.X, ev.Y, contextBase
    return,-1 ;don't pass on any events
  endif

  widget_control, ev.id, GET_UVALUE=uval

  retfile=''
  retpath=''

  ;Type 0 means select
  ;{WIDGET_TREE_SEL, ID:0L, TOP:0L, HANDLER:0L, TYPE:0, CLICKS:0L}

  ;Type 1 means expand or collapse (expand=0 on collapse)
  ;{WIDGET_TREE_EXPAND, ID:0L, TOP:0L, HANDLER:0L, TYPE:1, EXPAND:0L}

  case uval.type of
    'root':Begin  ;this should only generate events when we manually send an event to the root id.
      ;use this to avoid generating a string of events during the selection of the intial path.
      ev_name=TAG_NAMES( ev, /STRUCTURE_NAME )
      case ev_name of
        'BGDIRTREE_IGNORE' : self.ignore = 1
        'BGDIRTREE_ENABLE' : self.ignore =0
        else:
      endcase
      return,-1
    end
    'folder' : begin
      ;search for all expanded folders and refresh the contents
      if self.debug then begin
        if ev.type eq 0 then type='select' else begin
          if ev.expand eq 1 then type='expand' else type='collapse'
        endelse
        message,'Folder '+type+' event '+uval.path,/cont
      endif

      case ev.type of
        0:begin ;a folder was selected
          ;         if count ne 0 then widget_control,ev.id,/SET_TREE_EXPANDED
          retfile=''
          retpath=uval.path
        end
        1:begin ;a folder was expanded or collapsed
          if ev.expand eq 1 then begin  ;check the children folders and fill if necessary
            widget_control,self.tlb,/hourglass
            widget_control,self.tree_id,update=0,map=0
            ;            if self.debug then tic,/profiler
            self->refresh,ev.id,self.filter
            ;            if self.debug then toc
            ;            if self.debug then profiler,/report
            widget_control,self.tlb,hourglass=0
            widget_control,self.tree_id,update=1,map=1
          endif else begin
          endelse
          return,-1 ;don't return an event for expand or collapse
        end
        else:
      endcase
    end
    'file':begin
      case ev.type of
        0:begin ;a file was selected. Skip the refresh step if the file exists.
          filename=uval.path
          if ~file_test(filename) then begin
            widget_control,self.tlb,/hourglass
            widget_control,self.tree_id,update=0
            self->refresh,widget_info(ev.id,/parent),self.filter
            widget_control,self.tlb,hourglass=0
            widget_control,self.tree_id,update=1
            retfile=''
            retpath=file_dirname(filename,/mark_dir)
          endif else begin
            retfile=file_basename(filename)
            retpath=file_dirname(filename,/mark_dir)
          endelse
        end
        else:
      endcase
    end
    else:
  endcase

  sel = widget_info(self.tree_id,/tree_select)
  nsel = n_elements(sel)

  ret = { BGDirTree_EVENT, ID:self.tlb, TOP:ev.top, HANDLER:ev.top, $
    FILE:retfile, path:retpath, node_id:ev.id, n_selected:nsel, object:self }   ;Create an event.
  if self.ignore then ret=-1  ;don't send an event during the initial phase

  RETURN, ret
end

pro BGDirTree::RealizeNotify
  COMPILE_OPT HIDDEN
  wTree=self.tree_id

  ;if not in rooted mode, list all the available drives and add them to the tree
  if self.rooted eq 0 then begin
    drives=bg_get_Drives()

    ;create a disk icon. Read_tiff sometimes causes problems in VM mode.
    myIcon=[[255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,0,0,0,0,0,0,0,0,0,0,0,0,0,255,255,$
      115,115,115,115,115,115,115,115,115,115,115,115,115,115,0,255,$
      115,214,255,255,255,255,255,255,255,255,255,255,181,148,115,0,$
      115,222,115,115,115,115,115,115,115,115,115,115,181,148,115,0,$
      115,222,181,198,198,198,198,198,181,181,181,181,181,148,115,0,$
      115,214,198,198,198,198,198,198,198,181,0,0,181,148,115,0,$
      115,255,255,255,255,255,255,255,255,255,255,255,255,156,115,0,$
      255,115,181,181,181,181,181,181,181,181,181,181,181,181,115,0,$
      255,255,115,115,115,115,115,115,115,115,115,115,115,115,115,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255],$

      [255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,0,0,0,0,0,0,0,0,0,0,0,0,0,255,255,$
      115,115,115,115,115,115,115,115,115,115,115,115,115,115,0,255,$
      115,214,255,255,255,255,255,255,255,255,255,255,181,148,115,0,$
      115,222,115,115,115,115,115,115,115,115,115,115,181,148,115,0,$
      115,222,181,198,198,198,198,198,181,181,181,181,181,148,115,0,$
      115,214,198,198,198,198,198,198,198,181,206,132,181,148,115,0,$
      115,255,255,255,255,255,255,255,255,255,255,255,255,156,115,0,$
      255,115,181,181,181,181,181,181,181,181,181,181,181,181,115,0,$
      255,255,115,115,115,115,115,115,115,115,115,115,115,115,115,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255],$

      [255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,0,0,0,0,0,0,0,0,0,0,0,0,0,255,255,$
      115,115,115,115,115,115,115,115,115,115,115,115,115,115,0,255,$
      115,214,255,255,255,255,255,255,255,255,255,255,181,148,115,0,$
      115,222,115,115,115,115,115,115,115,115,115,115,181,148,115,0,$
      115,222,181,198,198,198,198,198,181,181,181,181,181,148,115,0,$
      115,214,198,198,198,198,198,198,198,181,0,0,181,148,115,0,$
      115,255,255,255,255,255,255,255,255,255,255,255,255,156,115,0,$
      255,115,181,181,181,181,181,181,181,181,181,181,181,181,115,0,$
      255,255,115,115,115,115,115,115,115,115,115,115,115,115,115,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,$
      255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255]]

    myIcon=reform(myIcon,16,16,3)

    ; file=programrootdir()+'disk_icon.tif'
    ;
    ; n_errors=0
    ;   CATCH, Error_status
    ;
    ;   ;This statement begins the error handler to catch the problem that sometimes
    ;   ;happens with VM save files where IDL complains that READ_TIFF is not defined
    ;   IF Error_status NE 0 THEN BEGIN
    ;      if n_errors gt 0 then begin;something else must be wrong. abort.
    ;       result=dialog_message('There was a problem in BGDirTree::RealizeNotify',/err)
    ;       return
    ;       endif
    ;      file=''  ;this will skip reading in the icon
    ;      n_errors++
    ;      CATCH, /CANCEL
    ;   ENDIF
    ;
    ; if file_test(file) then myIcon = reverse(READ_TIFF(file, INTERLEAVE=2),2)

　
    ;add the roots for the drives
    widget_control,wTree,update=0
    for i=0,n_elements(drives)-1 do begin
      wtRoot = WIDGET_TREE(wTree, VALUE=drives[i], /FOLDER, bitmap=myicon,$
        uvalue={Method:"MainEvents", Object:self, path:drives[i]+path_sep(), type:'folder'})  ;stash:self.stash,
      ;see if the folder has any contents
      count=self->add(wtRoot,self.filter)
    endfor

  endif else begin  ;run in rooted mode, so only show the tree below the current path.
    widget_control,wTree,update=0
    wtRoot = WIDGET_TREE(wTree, VALUE=self.initial_path, /FOLDER, $
      uvalue={Method:"MainEvents", Object:self, path:self.initial_path, type:'folder'})  ;stash:self.stash,
    ;see if the folder has any contents
    count=self->add(wtRoot,self.filter)

  endelse
  self->select,self.initial_path
  widget_control,wTree,/update

  ;now there will be a few events in the queue if an initial path was selected.
  ;send an event at the end of the queue to set self.ignore to 0, so that subsequent events get passed on.
  widget_control,wTree,send_event={BGDirTree_enable, id:wTree, top:self.tlb, handler:self.tlb, Method:"MainEvents", Object:self}
end

pro BGDirTree_Realize_Notify,id
  COMPILE_OPT HIDDEN
  widget_control,id,get_uval=uval
  (uval.object)->RealizeNotify
end

pro BGDirTree::refresh,id,filter  ;find all expanded folders under id and refresh them
  COMPILE_OPT HIDDEN

  if not widget_info(id,/valid) then return
  widget_control,id,get_uvalue=uval,/no_copy
  if uval.type ne 'folder' and uval.type ne 'root' then begin
    if self.debug then message,'Nothing to refresh',/cont
    widget_control,id,set_uvalue=uval,/no_copy
    return
  endif
  if self.debug then message,'Refreshing '+uval.path,/cont

  widget_control,id,set_uvalue=uval
  if n_elements(filter) eq 0 then filter=self.filter
  ;first, refresh the current directory
  count=self->add(id,filter)

  if widget_info(id,/tree_expanded) or (uval.type eq 'root') then begin
    ;find any expanded children, and recurse
    child=widget_info(id,/child)
    while child ne 0 do begin
      self->refresh,child,filter
      child=widget_info(child,/sibling)
    endwhile
  endif
end

Pro BGDirTree::SetProperty, $
  ; This method allows you to set various properties of the compound widget.
  filter=filter,$
  uvalue=uvalue

  COMPILE_OPT HIDDEN

  IF N_Elements(uvalue) NE 0 THEN *self.uvalue = uvalue
  if n_elements(filter) eq 1 then begin
    stop
    ;TODO - need to figure out how to clear previous files
    self.filter=filter
    widget_control,self.tree_id,update=0
    self->refresh,self.tree_ID
    widget_control,self.tree_id,update=1
  endif

end

Pro BGDirTree::set_value,value
  COMPILE_OPT HIDDEN

  message,'set_value method not implemented!',/cont

end

pro BGDirTree_set_value,id,value
  COMPILE_OPT HIDDEN
  ;an interface to the set_value method for use by external programs calling widget_control.

  stash=widget_info(id,/child)
  widget_control,stash,get_uvalue=uval
  obj=uval.object

  obj->set_value,value
end

pro BGDirTree::setRoot,path,filter=filter
  ;reset the root directory if in rooted mode

  if self.rooted then begin
    self->empty,self.tree_ID
    widget_Control,self.tree_ID,set_uval={Method:"MainEvents", Object:self, path:path, type:'root'}

    self.initial_path=path
    if n_elements(filter) eq 1 then self.filter=filter
    widget_control,self.tree_ID,update=0
    id = WIDGET_TREE(self.tree_ID, VALUE=self.initial_path, /FOLDER, $
      uvalue={Method:"MainEvents", Object:self, path:self.initial_path, type:'folder'})  ;stash:self.stash,
    ;see if the folder has any contents
    count=self->add(id,self.filter)
    self->select,self.initial_path
    widget_control,self.tree_id,/update
  endif
  return
end

pro BGDirTree::select,path  ;select a file or folder. expand any parent folders.
  COMPILE_OPT HIDDEN

  if path eq '' then return

  if not file_test(path) then return

  ;start at the root, then look for each folder in turn. If the folder doesn't exist yet,
  ;then fill it.

  if file_test(path,/dir) then folder=1 else folder=0

　
  parts=strsplit(path,path_sep(),/extract,count=count)

  this_node=0L

  ;first, select the first drive in the tree
  folderID=widget_info(self.tree_id,/child)

  if folderID eq 0 then return

  widget_control,self.tree_id,update=0
  for i=0,count-1 do begin  ;step through the path parts, and see if any siblings match
    expand=0
    if i eq count-1 then select=1 else select=0   ;only select the final item in the path.

    while folderID ne 0 do begin  ;search the siblings and find a match until no more siblings (folderID=0)
      widget_control,folderID,get_value=name

      if strlowcase(name) eq strlowcase(parts[i]) then begin  ;found a match
        expand=1
        break ; go to next part
      endif
      ;go to the next sibling folder
      folderID=widget_info(folderID,/sibling)
    endwhile

    ;either a match was found for the part, or we ran out of siblings
    if folderID eq 0 then return  ;no match was found

    ;go down a level
    result=self->add(folderID,self.filter,/full)  ;refresh the folder contents.

    ;expand the folder now that there are files in it, and select the file
    widget_control,folderID,/set_tree_expanded,set_tree_select=select

    folderID=widget_info(folderID,/child)
    if folderID eq 0 then begin
      widget_control,self.tree_id,update=1
      return
    endif
  endfor
  widget_control,self.tree_id,update=1

end

function BGDirTree::Init,$
  parent,$
  path=path,$
  filter=filter,$
  xsize=xsize,$
  ysize=ysize,$
  multiple=mult,$
  uvalue=uvalue,$
  uname=uname,$
  sensitive=sensitive,$
  frame=frame,$
  files_only=files_only,$
  event_func=event_func,$
  event_pro=event_pro,$
  debug=debug,$
  rooted=rooted,$
  _extra=extra

  COMPILE_OPT HIDDEN
  if n_elements(mult) eq 0 then mult=0 else mult=1
  if n_elements(filter) eq 0 then filter='' else filter=filter[0]
  if n_elements(path) eq 0 then path=''
  if not keyword_set(files_only) then files_only=0
  if n_elements(uvalue) eq 0 then uvalue=''
  if n_elements(event_func) eq 0 then event_func=''
  if n_elements(event_pro) eq 0 then event_pro=''
  if keyword_set(debug) then debug=1 else debug=0
  if keyword_Set(rooted) then rooted=1 else rooted=0

  if float(!version.release) le 5.0 then begin
    message,'BGDirTree requires IDL5.1 or greater.',/info
    return,0
  endif

  base = WIDGET_BASE(parent,/col, UVALUE=uvalue, uname=uname, sensitive=sensitive, frame=frame, $
    event_func=event_func, event_pro=event_pro,$
    FUNC_GET_VALUE='BGDirTree_get_value', PRO_SET_VALUE='BGDirTree_set_value')
  BASE_ID=WIDGET_BASE(base,/row,XPAD=2,YPAD=2,kill_notify='BGDirTree_Kill_Notify',$
    notify_realize='BGDirTree_Realize_Notify',EVENT_FUNC='BGDirTree_event_handler',$
    UValue={Method:"MainEvents", Object:self} )
  tree_ID = WIDGET_TREE(BASE_ID,mult=mult,xsize=xsize,ysize=ysize,/context_events,$
    uval={Method:"MainEvents", Object:self, path:'', type:'root'})  ;stash:base_ID,
  context_ID = widget_base(base_ID,/context_menu,UNAME = 'contextMenu')
  x = widget_button(context_ID, value='Select All', uval={Method:"ContextEvents", Object:self, path:'', type:'select all'}) ;stash:base_ID,
  x = widget_button(context_ID, value='Refresh', uval={Method:"ContextEvents", Object:self, path:'', type:'refresh'})       ;stash:base_ID,
  x = widget_button(context_ID, /separator, value='Rename', uval={Method:"ContextEvents", Object:self, path:'', type:'rename'})   ;stash:base_ID,
  x = widget_button(context_ID, value='Delete', uval={Method:"ContextEvents", Object:self, path:'', type:'delete'})       ;stash:base_ID,

  self.parent=parent
  self.TLB=BASE
  ; self.stash=base_ID
  self.tree_ID=tree_ID
  self.event_func=event_func
  self.event_pro=event_pro
  self.ignore=1
  self.rooted=rooted
  self.files_only=files_only
  self.initial_path=path
  self.filter=filter
  self.debug=debug
  self.uvalue=ptr_new(uvalue)

  RETURN, 1
end

Pro BGDirTree__Define
  COMPILE_OPT HIDDEN
  ;Define the BGDirTree widget object
  object_class={BGDirTree,$
    parent:0L,$
    tlb:0L,$
    tree_ID:0L,$
    event_func: '',$
    event_pro: '',$
    ignore:0,$
    files_only:0,$
    initial_path:'',$
    filter:'',$
    debug:0,$
    rooted:0,$
    uvalue:ptr_new()}

  ; The BGDirTree Event Structure. Sent only if EVENT_PRO or EVENT_FUNC keywords
  ; have defined an event handler for the top-level base of the compound widget.
  event = { BGDirTree_EVENT,$
    ID:0L,$
    TOP:0L,$
    HANDLER:0L, $
    FILE:'',$
    path:'',$
    node_id:0L,$
    n_selected:0L,$
    object:obj_new() }
end

function BGDirTree_widget,$
  parent,$
  path=path,$
  filter=filter,$
  xsize=xsize,$
  ysize=ysize,$
  multiple=mult,$
  uvalue=uvalue,$
  uname=uname,$
  sensitive=sensitive,$
  frame=frame,$
  files_only=files_only,$
  event_func=event_func,$
  event_pro=event_pro,$
  _extra=extra

  COMPILE_OPT HIDDEN

  Return, obj_new("BGDirTree",$
    parent,$
    path=path,$
    filter=filter,$
    xsize=xsize,$
    ysize=ysize,$
    multiple=mult,$
    uvalue=uvalue,$
    uname=uname,$
    sensitive=sensitive,$
    frame=frame,$
    files_only=files_only,$
    _extra=extra)

end

pro example_event,ev
  widget_control,ev.id,get_uvalue=uval
  widget_control,ev.top,get_uvalue=info
  case uval of
    'filter':begin
      filter=ev.objref->get_value()
      info.obj->setProperty,filter=filter
    end
    'file': begin
      ;Option 1, use the event structure directly
      help,ev,/str

      ;Option 2, get the widget value
      widget_control,ev.id,get_value=value
      print,value.n_selected,' files selected:',value.paths

      ;Option 3, call object methods
      obj=ev.object
      ;     obj->refresh  ;call a method on the widget object.
    end
    'done': widget_control,ev.top,/dest
  endcase
end

pro example

  path=programrootdir()

  tlb=WIDGET_BASE(/col,title='BGDirTree_widget Example')
  obj=BGDirTree_Widget(tlb,filter='*.pro',uval='file',path=path,ysize=500,xsize=300,/multiple,/rooted);,/debug)
  ;   filter_obj= FSC_INPUTFIELD(tlb, Title='Filter', Value='*.pro', /StringValue,/cr_only,event_pro='example_event',uvalue='filter')
  id=widget_button(tlb,value='Done',uvalue='done')

  widget_control,tlb,/real

  info={tlb:tlb,$
    obj:obj}
  widget_control,tlb,set_uvalue=info

  xmanager,'example',tlb

end
