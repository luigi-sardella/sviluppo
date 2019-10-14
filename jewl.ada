------------------------------------------------------------------------------
--                                                                          --
--             J E W L . C A N V A S _ I M P L E M E N T A T I O N          --
--                                                                          --
--   This is the body of a private package containing the internal          --
--   implementation details of canvases, as defined in JEWL.Windows.        --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-canvas_implementation.adb 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-canvas_implementation.adb $
-- Revision 1.7  2007/01/08 17:00:00  JE
-- * Fixed linker options in JEWL.Win32_Interface to accommodate changes to GNAT
--   GPL 2006 compiler (thanks to John McCormick for this)
-- * Added delay in message loop to avoid the appearance of hogging 100% of CPU
--   time
--
-- Revision 1.6  2001/11/02 16:00:00  JE
-- * Fixed canvas bug when saving an empty canvas
-- * Restore with no prior save now acts as erase
-- * Removed redundant variable declaration in Image function
--
-- Revision 1.5  2001/08/22 15:00:00  JE
-- * Minor bugfix to Get_Text for combo boxes
-- * Minor changes to documentation (including new example involving dialogs)
--
-- Revision 1.4  2001/01/25 09:00:00  je
-- Changes visible to the user:
--
-- * Added support for drawing bitmaps on canvases (Draw_Image operations
--   and new type Image_Type)
-- * Added Play_Sound
-- * Added several new operations on all windows: Get_Origin, Get_Width,
--   Get_Height, Set_Origin, Set_Size and Focus
-- * Added several functions giving screen and window dimensions: Screen_Width,
--   Screen_Height, Frame_Width, Frame_Height, Dialog_Width, Dialog_Height and
--   Menu_Height
-- * Canvases can now handle keyboard events: new constructor and Key_Code added
-- * Added procedure Play_Sound
-- * Operations "+" and "-" added for Point_Type
-- * Pens can now be zero pixels wide
-- * The absolute origin of a frame can now have be specified when the frame
--   is created
-- * Added new File_Dialog operations Add_Filter and Set_Directory
-- * Added Get_Line renames to JEWL.IO for compatibility with Ada.Text_IO
-- * Added all the Get(File,Item) operations mentioned in documentation but
--   unaccountably missing :-(
-- * Documentation updated to reflect the above changes
-- * HTML versions of public package specifications added with links from
--   main documentation pages
--
-- Other internal changes:
--
-- * Canvas fonts, pens etc. now use JEWL.Reference_Counted_Type rather than
--   reinventing this particular wheel, as do images
-- * Various minor code formatting changes: some code reordered for clarity,
--   some comments added or amended,
-- * Changes introduced in 1.2 to support GNAT 3.10 have been reversed, since
--   GNAT 3.10 still couldn't compile this code correctly... ;-(
--
-- Outstanding issues:
--
-- * Optimisation breaks the code (workaround: don't optimise)
--
-- Revision 1.3  2000/07/07 12:00:00  je
-- * JEWL.Simple_Windows added; JEWL.IO modified to use JEWL.Simple_Windows.
-- * JEWL.IO bug fix: Put_Line to file wrote newline to standard output
--   instead of to the file (thanks to Jeff Carter for pointing this out).
-- * Panels fixed so that mouse clicks are passed on correctly to subwindows.
-- * Memos fixed so that tabs are handled properly.
-- * Password feature added to editboxes.
-- * Minor typos fixed in comments within the package sources.
-- * Documentation corrected and updated following comments from Moti Ben-Ari
--   and Don Overheu.
--
-- Revision 1.2  2000/04/18 20:00:00  je
-- * Minor code changes to enable compilation by GNAT 3.10
-- * Minor documentation errors corrected
-- * Some redundant "with" clauses removed
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

package body JEWL.Canvas_Implementation is

  use JEWL.Win32_Interface;

  procedure Free is new Ada.Unchecked_Deallocation
                            (Canvas_Object_Type'Class, Canvas_Object_Ptr);

  ----------------------------------------------------------------------------
  --
  --                     C A N V A S _ M O N I T O R
  --
  --  This protected type mediates between the canvas operations and the
  --  message loop task.
  --
  ----------------------------------------------------------------------------

  protected body Canvas_Monitor is

    --------------------------------------------------------------------------
    --
    --  Clear: delete all objects on the drawing list by restoring to the
    --         beginning of the list.
    --
    procedure Clear is
    begin
      Save_Pointer := null;
      Restore;
    end Clear;

    --------------------------------------------------------------------------
    --
    --  Save: record the current position in the drawing list.
    --
    procedure Save is
    begin
      Save_Pointer := Last_Object;
    end Save;

    --------------------------------------------------------------------------
    --
    --  Restore: truncate the drawing list back to the saved position (or
    --           the beginning, if the saved position is null) and delete
    --           any objects removed in the process.
    --
    procedure Restore is
      P,Q : Canvas_Object_Ptr;
    begin
      if Save_Pointer = null then
        P := First_Object;
        First_Object := null;
        Last_Object  := null;
      else
        Last_Object := Save_Pointer;
        P := Last_Object.Next;
        Last_Object.Next := null;
      end if;
      while P /= null loop
        Q := P;
        P := P.Next;
        Free (Q);
      end loop;
    end Restore;

    --------------------------------------------------------------------------
    --
    --  Draw: draw all objects on the drawing list.
    --
    procedure Draw (Handle : in Win32_HWND;
                    Font   : in Win32_HFONT) is
      P : Canvas_Object_Ptr := First_Object;
      D : Win32_HDC;
      H : Win32_HANDLE;
      S : aliased Win32_PAINTSTRUCT;
      I : Win32_INT;
      L : aliased Win32_LOGBRUSH;
      B : Win32_HBRUSH;
    begin
      L.lbStyle := BS_HOLLOW;
      B := CreateBrushIndirect(L'Unchecked_Access);

      -- Start drawing using the initial tool set: a transparent brush,
      -- standard black pen, and the canvas font

      D := BeginPaint (Handle, S'Access);
      H := SelectObject(D, B);
      H := SelectObject(D, GetStockObject(BLACK_PEN));
      H := SelectObject(D, Font);
      I := SetBkMode (D, TRANSPARENT);

      -- Draw the objects in the drawing list

      while P /= null loop
        Draw (P.all, D);
        P := P.Next;
      end loop;

      -- Finish painting and destroy the brush

      Bool_Dummy := EndPaint (Handle, S'Access);
      Bool_Dummy := DeleteObject (B);
    end Draw;

    --------------------------------------------------------------------------
    --
    --  Add: add a new object to the end of the drawing list.
    --
    procedure Add (Object : in Canvas_Object_Ptr) is
    begin
      if Last_Object = null then
        First_Object := Object;
      else
        Last_Object.Next := Object;
      end if;
      Last_Object := Object;
      Object.Next := null;
    end Add;

    --------------------------------------------------------------------------
    --
    --  Set_Brush: store the brush used to erase the background.
    --
    procedure Set_Brush (Brush : in Win32_HBRUSH) is
    begin
      Bool_Dummy := DeleteObject(BG_Brush);
      BG_Brush := Brush;
    end Set_Brush;

    --------------------------------------------------------------------------
    --
    --  Background: get the background brush. This is called by the message
    --              loop task in response to a WM_ERASEBKGND message.
    --
    function Background return Win32_HBRUSH is
    begin
      return BG_Brush;
    end Background;

    --------------------------------------------------------------------------
    --
    --  Set_Start: store the position where the mouse button was pressed.
    --             This is called by the message loop task in response to
    --             a WM_LBUTTONDOWN message. The end position is initially
    --             set to match the start position.
    --
    procedure Set_Start (X, Y : in Integer) is
    begin
      Start_X := X;
      Start_Y := Y;
      End_X := X;
      End_Y := Y;
      Moved := False;
    end Set_Start;

    --------------------------------------------------------------------------
    --
    --  Get_Start: get the position where the mouse button was pressed.
    --
    procedure Get_Start (X, Y : out Integer) is
    begin
      X := Start_X;
      Y := Start_Y;
    end Get_Start;

    --------------------------------------------------------------------------
    --
    --  Set_End: store the current mouse position. This is called by
    --           the message loop task in response to a WM_MOUSEMOVE
    --           or WM_LBUTTONUP message. The Moved flag is set true
    --           to indicate that the mouse has moved.
    --
    procedure Set_End (X, Y : in Integer) is
    begin
      End_X := X;
      End_Y := Y;
      Moved := True;
    end Set_End;

    --------------------------------------------------------------------------
    --
    --  Get_End: get the current mouse position. The Moved flag is reset
    --           so that Mouse_Moved will return False until the mouse is
    --           moved again.
    --
    procedure Get_End (X, Y : out Integer) is
    begin
      X := End_X;
      Y := End_Y;
      Moved := False;
    end Get_End;

    --------------------------------------------------------------------------
    --
    --  Set_Button: store the current mouse button state. This is called
    --              from the message loop task in response to WM_LBUTTONUP
    --              or WM_LBUTTONDOWN messages.
    --
    procedure Set_Button (B : in Boolean) is
    begin
      Button := B;
    end Set_Button;

    --------------------------------------------------------------------------
    --
    --  Mouse_Down: get the current mouse button state.
    --
    function Mouse_Down return Boolean is
    begin
      return Button;
    end Mouse_Down;

    --------------------------------------------------------------------------
    --
    --  Mouse_Moved: test if the mouse has moved since the last time that
    --               Get_End was called.
    --
    function Mouse_Moved return Boolean is
    begin
      return Moved;
    end Mouse_Moved;

    --------------------------------------------------------------------------
    --
    --  Set_Key: store the character corresponding to a key that has been
    --           pressed. This is called from the message loop in response
    --           to WM_CHAR messages.
    --
    procedure Set_Key (C : in Character) is
    begin
      Keycode := C;
    end Set_Key;

    --------------------------------------------------------------------------
    --
    --  Get_Key: return the character corresponding to a key that has been
    --           pressed.
    --
    procedure Get_Key (C : out Character) is
    begin
      C := Keycode;
      Keycode := ASCII.NUL;
    end Get_Key;

  end Canvas_Monitor;

  ----------------------------------------------------------------------------
  --
  --                 D R A W I N G   O P E R A T I O N S
  --
  --  The following procedures are the implementations of Draw for the
  --  different types of canvas objects. They are called by Draw in the
  --  canvas monitor, which dispatches to the appropriate procedure for
  --  each object in the drawing list.
  --
  ----------------------------------------------------------------------------
  --
  --  Draw a text string
  --
  procedure Draw (Object : in out Text_Type;
                  Window : in Win32_HDC) is
    I : Win32_INT;
    R : aliased Win32_RECT;
    W : Win32_UINT;
    S : Win32_String := To_Array(Object.Text);
  begin
    -- Calculate the bounding rectangle

    R.Left   := Win32_LONG(Object.From.X);
    R.Top    := Win32_LONG(Object.From.Y);
    R.Right  := Win32_LONG(Object.To.X);
    R.Bottom := Win32_LONG(Object.To.Y);

    -- Select the appropriate alignment flag (-1 is used to indicate
    -- that the text is not clipped by the bounding rectangle, and 0
    -- upwards are values generated by Alignment_Type'Pos).

    if Object.Align < 0 then
      W := DT_NOCLIP;
    elsif Object.Align = 0 then
      W := DT_LEFT;
    elsif Object.Align = 1 then
      W := DT_CENTER;
    else
      W := DT_RIGHT;
    end if;

    -- Now draw the text

    I := DrawText (Window, To_LPCSTR(S), Win32_INT(Object.Length),
                   R'Unchecked_Access, W);
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a line
  --
  procedure Draw (Object : in out Line_Type;
                  Window : in Win32_HDC) is
  begin
    Bool_Dummy := MoveToEx (Window, Win32_INT(Object.From.X),
                                    Win32_INT(Object.From.Y),
                                    null);
    Bool_Dummy := LineTo (Window, Win32_INT(Object.To.X),
                                  Win32_INT(Object.To.Y));
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a rectangle
  --
  procedure Draw (Object : in out Rectangle_Type;
                  Window : in Win32_HDC) is
  begin
    Bool_Dummy := Rectangle (Window, Win32_INT(Object.From.X),
                                     Win32_INT(Object.From.Y),
                                     Win32_INT(Object.To.X),
                                     Win32_INT(Object.To.Y));
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a rectangle with rounded corners
  --
  procedure Draw (Object : in out Rounded_Rectangle_Type;
                  Window : in Win32_HDC) is
  begin
    Bool_Dummy := RoundRect (Window, Win32_INT(Object.From.X),
                                     Win32_INT(Object.From.Y),
                                     Win32_INT(Object.To.X),
                                     Win32_INT(Object.To.Y),
                                     Win32_INT(Object.Corner.X),
                                     Win32_INT(Object.Corner.Y));
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw an ellipse
  --
  procedure Draw (Object : in out Ellipse_Type;
                  Window : in Win32_HDC) is
  begin
    Bool_Dummy := Ellipse (Window, Win32_INT(Object.From.X),
                                   Win32_INT(Object.From.Y),
                                   Win32_INT(Object.To.X),
                                   Win32_INT(Object.To.Y));
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a polyline
  --
  procedure Draw (Object : in out Polyline_Type;
                  Window : in Win32_HDC) is
  begin
    Bool_Dummy := Polyline (Window, Object.Points(1)'Unchecked_Access,
                                    Win32_INT(Object.Count));
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a polygon
  --
  procedure Draw (Object : in out Polygon_Type;
                  Window : in Win32_HDC) is
  begin
    Bool_Dummy := Polygon (Window, Object.Points(1)'Unchecked_Access,
                                   Win32_INT(Object.Count));
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Draw a bitmap
  --
  procedure Draw (Object : in out Bitmap_Type;
                  Window : in Win32_HDC) is
    H : Win32_HDC := CreateCompatibleDC (Window);
    B : Win32_BITMAP;
    P : aliased Win32_POINT;
    Q : aliased Win32_POINT;
    I : Image_Ptr := Image_Ptr(Object.Bitmap.Pointer);
    W : Win32_HBITMAP := I.Image;
    N : Win32_INT;
  begin
    Long_Dummy := To_LONG (SelectObject (H, W));
    N := SetMapMode (H, GetMapMode(Window));
    N := GetObject (W, Win32_INT(Win32_BITMAP'Size/Win32_BYTE'Size),
                    B'Address);
    P := (X => B.bmWidth, Y => B.bmHeight);
    Bool_Dummy := DPtoLP (Window, P'Unchecked_Access, 1);
    Q := (0,0);
    Bool_Dummy := DPtoLP (H, Q'Unchecked_Access, 1);
    Bool_Dummy := StretchBlt (Window,
                              Win32_INT(Object.From.X), Win32_INT(Object.From.Y),
                              Win32_INT(Object.Width), Win32_INT(Object.Height),
                              H,
                              Win32_INT(Q.X), Win32_INT(Q.Y),
                              Win32_INT(I.Width), Win32_INT(I.Height));
    Bool_Dummy := DeleteDC (H);
  end Draw;

  ----------------------------------------------------------------------------
  --
  --  Select a drawing tool
  --
  procedure Draw (Object : in out Handle_Type;
                  Window : in Win32_HDC) is
    H : Win32_HGDIOBJ;
    W : Win32_HBITMAP :=
                Counted_Handle_Type(Object.Handle.Pointer.all).Handle;
  begin
    H := SelectObject (Window, W);
  end Draw;

  ----------------------------------------------------------------------------
  --
  --         C O N T R O L L E D   T Y P E   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Cleanup: destroy a bitmap handle in an Image_Internals object.
  --
  procedure Cleanup (Object : in out Image_Internals) is
  begin
    Bool_Dummy := DeleteObject (Object.Image);
  end Cleanup;

  ----------------------------------------------------------------------------
  --
  --  Cleanup: destroy a handle to a Windows GDI object.
  --
  procedure Cleanup (Object : in out Counted_Handle_Type) is
  begin
    Bool_Dummy := DeleteObject (Object.Handle);
  end Cleanup;

  ----------------------------------------------------------------------------
  --
  --  Handle: create a reference counted object for a Windows handle.
  --
  function  Handle (Object : Win32_HGDIOBJ) return JEWL.Controlled_Type is
    C : JEWL.Controlled_Type;
  begin
    C.Pointer := new Counted_Handle_Type;
    Counted_Handle_Type(C.Pointer.all).Handle := Object;
    return C;
  end Handle;

end JEWL.Canvas_Implementation;
------------------------------------------------------------------------------
--                                                                          --
--             J E W L . C A N V A S _ I M P L E M E N T A T I O N          --
--                                                                          --
--   This is a private package containing the internal implementation       --
--   details of the canvases, as defined in JEWL.Windows.                   --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-canvas_implementation.ads 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-canvas_implementation.ads $
-- Revision 1.7  2007/01/08 17:00:00  JE
-- * Fixed linker options in JEWL.Win32_Interface to accommodate changes to GNAT
--   GPL 2006 compiler (thanks to John McCormick for this)
-- * Added delay in message loop to avoid the appearance of hogging 100% of CPU
--   time
--
-- Revision 1.6  2001/11/02 16:00:00  JE
-- * Fixed canvas bug when saving an empty canvas
-- * Restore with no prior save now acts as erase
-- * Removed redundant variable declaration in Image function
--
-- Revision 1.5  2001/08/22 15:00:00  JE
-- * Minor bugfix to Get_Text for combo boxes
-- * Minor changes to documentation (including new example involving dialogs)
--
-- Revision 1.4  2001/01/25 09:00:00  je
-- Changes visible to the user:
--
-- * Added support for drawing bitmaps on canvases (Draw_Image operations
--   and new type Image_Type)
-- * Added Play_Sound
-- * Added several new operations on all windows: Get_Origin, Get_Width,
--   Get_Height, Set_Origin, Set_Size and Focus
-- * Added several functions giving screen and window dimensions: Screen_Width,
--   Screen_Height, Frame_Width, Frame_Height, Dialog_Width, Dialog_Height and
--   Menu_Height
-- * Canvases can now handle keyboard events: new constructor and Key_Code added
-- * Added procedure Play_Sound
-- * Operations "+" and "-" added for Point_Type
-- * Pens can now be zero pixels wide
-- * The absolute origin of a frame can now have be specified when the frame
--   is created
-- * Added new File_Dialog operations Add_Filter and Set_Directory
-- * Added Get_Line renames to JEWL.IO for compatibility with Ada.Text_IO
-- * Added all the Get(File,Item) operations mentioned in documentation but
--   unaccountably missing :-(
-- * Documentation updated to reflect the above changes
-- * HTML versions of public package specifications added with links from
--   main documentation pages
--
-- Other internal changes:
--
-- * Canvas fonts, pens etc. now use JEWL.Reference_Counted_Type rather than
--   reinventing this particular wheel, as do images
-- * Various minor code formatting changes: some code reordered for clarity,
--   some comments added or amended,
-- * Changes introduced in 1.2 to support GNAT 3.10 have been reversed, since
--   GNAT 3.10 still couldn't compile this code correctly... ;-(
--
-- Outstanding issues:
--
-- * Optimisation breaks the code (workaround: don't optimise)
--
-- Revision 1.3  2000/07/07 12:00:00  je
-- * JEWL.Simple_Windows added; JEWL.IO modified to use JEWL.Simple_Windows.
-- * JEWL.IO bug fix: Put_Line to file wrote newline to standard output
--   instead of to the file (thanks to Jeff Carter for pointing this out).
-- * Panels fixed so that mouse clicks are passed on correctly to subwindows.
-- * Memos fixed so that tabs are handled properly.
-- * Password feature added to editboxes.
-- * Minor typos fixed in comments within the package sources.
-- * Documentation corrected and updated following comments from Moti Ben-Ari
--   and Don Overheu.
--
-- Revision 1.2  2000/04/18 20:00:00  je
-- * Minor code changes to enable compilation by GNAT 3.10
-- * Minor documentation errors corrected
-- * Some redundant "with" clauses removed
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

with Ada.Finalization;
with JEWL.Win32_Interface;

private package JEWL.Canvas_Implementation is

  use JEWL.Win32_Interface;

  type Point_List is array (Positive range <>) of aliased Win32_POINT;

  ----------------------------------------------------------------------------
  --  Forward declarations (defined fully later).
  ----------------------------------------------------------------------------

  type Canvas_Object_Type;
  type Canvas_Object_Ptr is access Canvas_Object_Type'Class;

  type Image_Internals;
  type Image_Ptr is access all Image_Internals;

  ----------------------------------------------------------------------------
  --
  --                     C A N V A S _ M O N I T O R
  --
  --  Every canvas contains one of these protected records to synchronise
  --  drawing operations with accesses from the message loop task. The
  --  canvas stores the objects which are drawn on it as a linked list
  --  of drawing objects (defined in JEWL.Objects).
  --
  --  The private data of a Canvas_Monitor is as follows:
  --
  --  First_Object : pointer to the first object in the drawing list
  --  Last_Object  : pointer to the last object in the drawing list
  --  Save_Pointer : pointer to the saved position in the drawing list
  --  BG_Brush     : the brush used to paint the background of the canvas
  --  Start_X      : the X position where the mouse button was pressed
  --  Start_Y      : the Y position where the mouse button was pressed
  --  End_X        : the most recent X position while the mouse is down
  --  End_Y        : the most recent Y position while the mouse is down
  --  Button       : true if the mouse is down
  --  Moved        : true is the mouse has moved while the button is down
  --  Keycode      : the key that was pressed (or NUL if none)
  --
  --  The operations on a Canvas_Monitor are as follows:
  --
  --  Clear        : delete all objects in the drawing list
  --  Save         ; save a pointer to the current end of the drawing list
  --  Restore      : truncate the drawing list to a previously saved point
  --  Draw         : draw all objects in the drawing list (called from the
  --                 message loop task)
  --  Add          : add an object to the end of the drawing list
  --  Set_Brush    : set the background brush
  --  Background   : get the background brush (called from the message loop
  --                 task)
  --  Set_Start    : set the start position when the mouse button is pressed
  --                 (called from the message loop task)
  --  Get_Start    : get the start position when the mouse button is pressed
  --  Set_End      : set the current mouse position and the mouse-move flag
  --                 (called from the message loop task)
  --  Get_End      : get the current mouse position
  --  Set_Button   : set the mouse button state (called from the message loop
  --                 task)
  --  Mouse_Down   : return the mouse button state
  --  Mouse_Moved  : return the mouse-move flag and reset it
  --  Set_Key      : set the keycode for a key press
  --  Get_Key      : get and reset the current keycode
  --
  ----------------------------------------------------------------------------

  protected type Canvas_Monitor is
    procedure Clear;
    procedure Save;
    procedure Restore;
    procedure Draw       (Handle : in Win32_HWND;
                          Font   : in Win32_HFONT);
    procedure Add        (Object : in Canvas_Object_Ptr);
    procedure Set_Brush  (Brush : in Win32_HBRUSH);
    function  Background  return Win32_HBRUSH;
    procedure Set_Start  (X, Y : in  Integer);
    procedure Get_Start  (X, Y : out Integer);
    procedure Set_End    (X, Y : in  Integer);
    procedure Get_End    (X, Y : out Integer);
    procedure Set_Button (B : in  Boolean);
    function  Mouse_Down  return Boolean;
    function  Mouse_Moved return Boolean;
    procedure Set_Key    (C : in  Character);
    procedure Get_Key    (C : out Character);
  private
    First_Object : Canvas_Object_Ptr;
    Last_Object  : Canvas_Object_Ptr;
    Save_Pointer : Canvas_Object_Ptr;
    BG_Brush     : Win32_HBRUSH;
    Start_X      : Integer   := 0;
    Start_Y      : Integer   := 0;
    End_X        : Integer   := 0;
    End_Y        : Integer   := 0;
    Button       : Boolean   := False;
    Moved        : Boolean   := False;
    Keycode      : Character := ASCII.NUL;
  end Canvas_Monitor;

  ----------------------------------------------------------------------------
  --
  --                 C A N V A S _ O B J E C T _ T Y P E
  --
  --  Canvas_Object_Type is the base type from which all drawing objects
  --  are derived, and Canvas_Object_Ptr is a class-wide pointer. Each
  --  object contains a pointer so that a singly-linked list of objects
  --  can be constructed. Every object must define a Draw procedure which
  --  draws the object on a window (which is identified by a handle to a
  --  Windows display context).
  --
  ----------------------------------------------------------------------------

  type Canvas_Object_Type is abstract tagged
    record
      Next : Canvas_Object_Ptr;
    end record;

  procedure Draw (Object : in out Canvas_Object_Type;
                  Window : in Win32_HDC) is abstract;

  ----------------------------------------------------------------------------
  --
  --  Text_Type: an object type containing a text string
  --
  type Text_Type (Length : Natural) is new Canvas_Object_Type with
    record
      Text     : String (1..Length);
      From, To : JEWL.Point_Type;
      Align    : Integer;
    end record;

  procedure Draw (Object : in out Text_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  --  Line_Type: an object type which defines a line by a pair of endpoints.
  --
  type Line_Type is new Canvas_Object_Type with
    record
      From, To : JEWL.Point_Type;
    end record;

  procedure Draw (Object : in out Line_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  --  Rectangle_Type: an object type which defines a rectangle as a pair
  --                  of coordinates for two opposite corners.
  --
  type Rectangle_Type is new Canvas_Object_Type with
    record
      From, To : JEWL.Point_Type;
    end record;

  procedure Draw (Object : in out Rectangle_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  -- Rounded_Rectangle_Type: an object type which defines a rectangle with
  --                         rounded corners using a pair of coordinates
  --                         for two opposite corners and a point to define
  --                         the size of the arc used to round the corners.
  --
  type Rounded_Rectangle_Type is new Canvas_Object_Type with
    record
      From, To, Corner : Point_Type;
    end record;

  procedure Draw (Object : in out Rounded_Rectangle_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  -- Ellipse_Type: an object type which defines an ellipse or circle using
  --               a pair of coordinates for two opposite corners of the
  --               bounding rectangle.
  --
  type Ellipse_Type is new Canvas_Object_Type with
    record
      From, To : JEWL.Point_Type;
    end record;

  procedure Draw (Object : in out Ellipse_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  -- Polyline_Type: an object type which defines an open figure as a
  --                sequence of lines.
  --
  type Polyline_Type (Count : Positive) is new Canvas_Object_Type with
    record
      Points : Point_List(1..Count);
    end record;

  procedure Draw (Object : in out PolyLine_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  -- Polygon_Type: an object type which defines an open figure as a
  --               sequence of lines.
  --
  type Polygon_Type (Count : Positive) is new Canvas_Object_Type with
    record
      Points : Point_List(1..Count);
    end record;

  procedure Draw (Object : in out Polygon_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  -- Handle_Type: an object type which encapsulates a handle to a Windows
  --              GDI object (e.g. a pen or a brush).
  --
  type Handle_Type is new Canvas_Object_Type with
    record
      Handle : JEWL.Controlled_Type;
    end record;

  procedure Draw (Object : in out Handle_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  -- Bitmap_Type: an object type representing a bitmap image.
  --
  type Bitmap_Type is new Canvas_Object_Type with
    record
      From   : JEWL.Point_Type;
      Width  : Natural;
      Height : Natural;
      Bitmap : JEWL.Controlled_Type;
    end record;

  procedure Draw (Object : in out Bitmap_Type;
                  Window : in Win32_HDC);

  ----------------------------------------------------------------------------
  --
  --                   C O N T R O L L E D   T Y P E S
  --
  --  Each type below is a reference counted type which contains a handle to
  --  a Windows GDI object. The handle will be deleted automatically when the
  --  last object which refers to it is destroyed.
  --
  ----------------------------------------------------------------------------
  --
  --  Image_Internals: used to store bitmaps in drawings
  --
  type Image_Internals is new Reference_Counted_Type with
    record
      Image  : Win32_HBITMAP;
      Width  : Natural;
      Height : Natural;
    end record;
    
  procedure Cleanup (Object : in out Image_Internals);

  ----------------------------------------------------------------------------
  --
  --  Counted_Handle_Type: used to store pens, brushes etc. in drawings
  --
  type Counted_Handle_Type is new JEWL.Reference_Counted_Type with
    record
      Handle : Win32_HGDIOBJ;
    end record;

  procedure Cleanup (Object : in out Counted_Handle_Type);

  ----------------------------------------------------------------------------
  --
  --  Handle: a function to create a controlled type object which contains
  --          a Counted_Handle_Type object.
  --
  function  Handle (Object : Win32_HGDIOBJ) return JEWL.Controlled_Type;

end JEWL.Canvas_Implementation;
------------------------------------------------------------------------------
--                                                                          --
--                             J E W L . I O                                --
--                                                                          --
--   This is the body of an extended input-output package for beginners.    --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-io.adb 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-io.adb $
-- Revision 1.7  2007/01/08 17:00:00  JE
-- * Fixed linker options in JEWL.Win32_Interface to accommodate changes to GNAT
--   GPL 2006 compiler (thanks to John McCormick for this)
-- * Added delay in message loop to avoid the appearance of hogging 100% of CPU
--   time
--
-- Revision 1.6  2001/11/02 16:00:00  JE
-- * Fixed canvas bug when saving an empty canvas
-- * Restore with no prior save now acts as erase
-- * Removed redundant variable declaration in Image function
--
-- Revision 1.5  2001/08/22 15:00:00  JE
-- * Minor bugfix to Get_Text for combo boxes
-- * Minor changes to documentation (including new example involving dialogs)
--
-- Revision 1.4  2001/01/25 09:00:00  je
-- Changes visible to the user:
--
-- * Added support for drawing bitmaps on canvases (Draw_Image operations
--   and new type Image_Type)
-- * Added Play_Sound
-- * Added several new operations on all windows: Get_Origin, Get_Width,
--   Get_Height, Set_Origin, Set_Size and Focus
-- * Added several functions giving screen and window dimensions: Screen_Width,
--   Screen_Height, Frame_Width, Frame_Height, Dialog_Width, Dialog_Height and
--   Menu_Height
-- * Canvases can now handle keyboard events: new constructor and Key_Code added
-- * Added procedure Play_Sound
-- * Operations "+" and "-" added for Point_Type
-- * Pens can now be zero pixels wide
-- * The absolute origin of a frame can now have be specified when the frame
--   is created
-- * Added new File_Dialog operations Add_Filter and Set_Directory
-- * Added Get_Line renames to JEWL.IO for compatibility with Ada.Text_IO
-- * Added all the Get(File,Item) operations mentioned in documentation but
--   unaccountably missing :-(
-- * Documentation updated to reflect the above changes
-- * HTML versions of public package specifications added with links from
--   main documentation pages
--
-- Other internal changes:
--
-- * Canvas fonts, pens etc. now use JEWL.Reference_Counted_Type rather than
--   reinventing this particular wheel, as do images
-- * Various minor code formatting changes: some code reordered for clarity,
--   some comments added or amended,
-- * Changes introduced in 1.2 to support GNAT 3.10 have been reversed, since
--   GNAT 3.10 still couldn't compile this code correctly... ;-(
--
-- Outstanding issues:
--
-- * Optimisation breaks the code (workaround: don't optimise)
--
-- Revision 1.3  2000/07/07 12:00:00  je
-- * JEWL.Simple_Windows added; JEWL.IO modified to use JEWL.Simple_Windows.
-- * JEWL.IO bug fix: Put_Line to file wrote newline to standard output
--   instead of to the file (thanks to Jeff Carter for pointing this out).
-- * Panels fixed so that mouse clicks are passed on correctly to subwindows.
-- * Memos fixed so that tabs are handled properly.
-- * Password feature added to editboxes.
-- * Minor typos fixed in comments within the package sources.
-- * Documentation corrected and updated following comments from Moti Ben-Ari
--   and Don Overheu.
--
-- Revision 1.2  2000/04/18 20:00:00  je
-- * Minor code changes to enable compilation by GNAT 3.10
-- * Minor documentation errors corrected
-- * Some redundant "with" clauses removed
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

with JEWL.Simple_Windows;   use JEWL.Simple_Windows;

with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Fixed;

package body JEWL.IO is

  ----------------------------------------------------------------------------
  --  Command codes
  ----------------------------------------------------------------------------

  OK     : constant Character := 'Y';
  Cancel : constant Character := 'Q';
  Yes    : constant Character := 'Y';
  No     : constant Character := 'N';

  ----------------------------------------------------------------------------
  --  Renaming of Trim function (for convenience)
  ----------------------------------------------------------------------------

  function Trim (Source : String;
                 Side   : Ada.Strings.Trim_End := Ada.Strings.Both)
           return  String
           renames Ada.Strings.Fixed.Trim;

  ----------------------------------------------------------------------------
  --
  --             M E S S A G E   B O X   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Error: Display an error message box and echo the message to the
  --         standard output.
  --
  procedure Error (Text : in String) is
  begin
    Ada.Text_IO.Put_Line ("Error: " & Text);
    Show_Error (Text, "Error");
  end Error;

  ----------------------------------------------------------------------------
  --
  --  Query: display a query message box and echo the message and response
  --         to the standard output.
  --
  function Query (Text : in String) return Boolean is
    B : Boolean;
  begin
    Ada.Text_IO.Put ("Query: " & Text & ' ');
    B := Show_Query (Text, "Query");
    if B then
      Ada.Text_IO.Put_Line ("YES");
    else
      Ada.Text_IO.Put_Line ("NO");
    end if;
    return B;
  end Query;

  ----------------------------------------------------------------------------
  --
  --  Message: display an information message box and echo the message to
  --           the standard output.
  --
  procedure Message (Text : in String) is
  begin
    Ada.Text_IO.Put_Line ("Message: " & Text);
    Show_Message (Text, "Message");
  end Message;

  ----------------------------------------------------------------------------
  --
  --                S T R I N G   I N P U T / O U T P U T
  --
  ----------------------------------------------------------------------------
  --
  --  Get: use a dialog box to get a string, echoing the input to the
  --       standard output. The input string is returned as the result
  --       of the function, or Input_Cancelled is raised if the Cancel
  --       button is pressed.
  --
  function Get (Prompt  : in  String := "Enter your text:";
                Default : in  String := "")
           return String is
    D : Dialog_Type  := Dialog (300, 150, "Input required", Cancel);
    P : Label_Type   := Label   (D, (15,15), -30, 30, Prompt);
    E : Editbox_Type := Editbox (D, (15,45), -30, 20, Default);
    Y : Button_Type  := Button  (D, (55,80), 80, 25, "OK", OK, True);
    N : Button_Type  := Button  (D, (165,80), 80, 25, "Cancel", Cancel);
  begin
    -- Echo the prompt, then execute the dialog

    Ada.Text_IO.Put (Prompt & ' ');
    if Execute(D) = OK then

      -- OK pressed: get the string from the dialog's editbox, echo it
      -- to the standard output and return it

      declare
        S : constant String := Get_Text(E);
      begin
        Ada.Text_IO.Put_Line(S);
        return S;
      end;

    else

      -- Cancel pressed: raise an exception

      raise Input_Cancelled;

    end if;
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Get: use a dialog box to get a string, echoing the input to the
  --       standard output. The string and its length are returned in
  --       the parameters provided.
  --
  procedure Get (Item    : out String;
                 Length  : out Natural;
                 Prompt  : in  String := "Enter your text:";
                 Default : in  String := "") is
    S : constant String := Get(Prompt,Default);
  begin
    if S'Length < Item'Length then

      -- String will fit into variable provided, so copy it padded
      -- with spaces

      Length := S'Length;
      Item := (others => ' ');
      Item(Item'First..Item'First+Length-1) := S;
      Ada.Text_IO.Put_Line(S);

    else

      -- String is too long, so truncate it to fit the size available

      Length := Item'Length;
      Item := S(S'First..S'First+Length-1);
      Ada.Text_IO.Put_Line(Item);

    end if;
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Put_Line: write a character and newline to the standard output.
  --
  procedure Put_Line (Item : in Character) is
  begin
    Put (Item);
    New_Line;
  end Put_Line;

  ----------------------------------------------------------------------------
  --
  --  Put_Line: write a character and newline to the specified file.
  --
  procedure Put_Line (File : in Ada.Text_IO.File_Type;
                      Item : in Character) is
  begin
    Put (File, Item);
    New_Line (File);
  end Put_Line;

  ----------------------------------------------------------------------------
  --
  --               I N T E G E R   I N P U T / O U T P U T
  --
  ----------------------------------------------------------------------------
  --
  --  Get: use a dialog box to get an Integer value and echo it to the
  --       standard output.
  --
  procedure Get (Item   : out Integer;
                 Prompt : in  String := "Enter an integer:") is
  begin
    loop
      begin
        Item := Integer'Value(Get(Prompt,""));
        return;
      exception
        when Constraint_Error =>
          JEWL.IO.Error("You must enter an integer.");
      end;
    end loop;
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Get: use a dialog box to get an Integer value using the supplied
  --       default value and echo it to the standard output.
  --
  procedure Get (Item    : out Integer;
                 Default : in  Integer;
                 Prompt  : in  String := "Enter an integer:") is
  begin
    loop
      begin
        Item := Integer'Value(Get(Prompt,Trim(Integer'Image(Default))));
        return;
      exception
        when Constraint_Error =>
          JEWL.IO.Error("You must enter an integer.");
      end;
    end loop;
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Get: get an Integer value from a file.
  --
  procedure Get (File : in  File_Type;
                 Item : out Integer) is
    package IO is new Ada.Text_IO.Integer_IO (Integer);
  begin
    IO.Get (File, Item);
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Put: write an Integer value to the standard output.
  --
  procedure Put (Item : in Integer) is
  begin
    Ada.Text_IO.Put (To_String(Item));
  end Put;

  ----------------------------------------------------------------------------
  --
  --  Put: write an Integer value to the specified file.
  --
  procedure Put (File : in Ada.Text_IO.File_Type;
                 Item : in Integer) is
  begin
    Ada.Text_IO.Put (File, To_String(Item));
  end Put;

  ----------------------------------------------------------------------------
  --
  --  Put_Line: write an Integer value and newline to the standard output.
  --
  procedure Put_Line (Item : in Integer) is
  begin
    Put (Item);
    New_Line;
  end Put_Line;

  ----------------------------------------------------------------------------
  --
  --  Put_Line: write an Integer value and newline to the specified file.
  --
  procedure Put_Line (File : in File_Type;
                      Item : in Integer) is
  begin
    Put (File, Item);
    New_Line (File);
  end Put_Line;

  ----------------------------------------------------------------------------
  --
  --  To_String: convert an Integer value to a string.
  --
  function To_String (Item : Integer) return String is
  begin
    return Trim(Integer'Image(Item));
  end To_String;

  ----------------------------------------------------------------------------
  --
  --  "&": concatenate a string and an Integer value.
  --
  function "&" (Left  : String;
                Right : Integer) return String is
  begin
    return Left & To_String(Right);
  end "&";

  ----------------------------------------------------------------------------
  --
  --  "&": concatenate an Integer value and a string.
  --
  function "&" (Left  : Integer;
                Right : String) return String is
  begin
    return To_String(Left) & Right;
  end "&";

  ----------------------------------------------------------------------------
  --
  --                 F L O A T   I N P U T / O U T P U T
  --
  ----------------------------------------------------------------------------
  --
  --  Get: use a dialog box to get a Float value and echo it to the standard
  --       output.
  --
  procedure Get (Item   : out Float;
                 Prompt : in  String := "Enter a number:") is
  begin
    loop
      begin
        Item := Float'Value(Get(Prompt,""));
        return;
      exception
        when Constraint_Error =>
          JEWL.IO.Error("You must enter a number.");
      end;
    end loop;
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Get: use a dialog box to get a Float value using the supplied default
  --       value and echo it to the standard output.
  --
  procedure Get (Item    : out Float;
                 Default : in  Float;
                 Prompt  : in  String := "Enter a number:") is
  begin
    loop
      begin
        Item := Float'Value(Get(Prompt,To_String(Default)));
        return;
      exception
        when Constraint_Error =>
          JEWL.IO.Error("You must enter a number.");
      end;
    end loop;
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Get: get a Float value from a file.
  --
  procedure Get (File : in  File_Type;
                 Item : out Float) is
    package IO is new Ada.Text_IO.Float_IO (Float);
  begin
    IO.Get (File, Item);
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Put: write a Float value to the standard output.
  --
  procedure Put (Item : in  Float) is
  begin
    Ada.Text_IO.Put(To_String(Item));
  end Put;

  ----------------------------------------------------------------------------
  --
  --  Put: write a Float value to the specified file.
  --
  procedure Put (File : in File_Type;
                 Item : in Float) is
  begin
    Ada.Text_IO.Put(File,To_String(Item));
  end Put;

  ----------------------------------------------------------------------------
  --
  --  Put_Line: write a Float value and newline to the standard output.
  --
  procedure Put_Line (Item : in Float) is
  begin
    Put (Item);
    New_Line;
  end Put_Line;

  ----------------------------------------------------------------------------
  --
  --  Put_Line: write a Float value and newline to the specified file.
  --
  procedure Put_Line (File : in File_Type;
                      Item : in Float) is
  begin
    Put (File, Item);
    New_Line (File);
  end Put_Line;

  ----------------------------------------------------------------------------
  --
  --  To_String: convert a Float value to a string.
  --
  function To_String (Item : Float) return String is
    S : String(1..Float'Width);
    N : Integer;
  begin
    Ada.Float_Text_IO.Put(S,Item,Exp=>0);
    for I in reverse S'Range loop
      N := I;
      exit when S(I) /= '0';
    end loop;
    if S(N) = '.' then
      N := N + 1;
    end if;
    return Trim(S(1..N));
  end To_String;

  ----------------------------------------------------------------------------
  --
  --  "&": concatenate a string and a Float value.
  --
  function "&" (Left  : String;
                Right : Float) return String is
  begin
    return Left & To_String(Right);
  end "&";

  ----------------------------------------------------------------------------
  --
  --  "&": concatenate a Float value and a string.
  --
  function "&" (Left  : Float;
                Right : String) return String is
  begin
    return To_String(Left) & Right;
  end "&";

  ----------------------------------------------------------------------------
  --
  --               B O O L E A N   I N P U T / O U T P U T
  --
  ----------------------------------------------------------------------------
  --
  --  Get: use a dialog box to get a Boolean value and echo it to the
  --       standard output. The dialog box uses Yes, No and Cancel
  --       buttons; Yes and No return true and false respectively,
  --       and Cancel raises an Input_Cancelled exception.
  --
  procedure Get (Item   : out Boolean;
                 Prompt : in  String := "Yes or no?") is
    D : Dialog_Type  := Dialog (330, 115, "Input required", Cancel);
    P : Label_Type   := Label   (D, (15,15), -30, 30, Prompt);
    Y : Button_Type  := Button  (D, (20,55), 80, 25, "&Yes", Yes, True);
    N : Button_Type  := Button  (D, (115,55), 80, 25, "&No", No);
    C : Button_Type  := Button  (D, (215,55), 80, 25, "Cancel", Cancel);
    E : Character;
  begin
    Ada.Text_IO.Put (Prompt & ' ');
    E := Execute(D);
    if E /= Cancel then
      Item := (E = OK);
      Put (Item);
      New_Line;
    else
      raise Input_Cancelled;
    end if;
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Get: use a dialog box to get a Boolean value using the supplied
  --       default and echo it to the standard output. The dialog box
  --       uses a checkbox set to the default value.
  --
  procedure Get (Item    : out Boolean;
                 Default : in  Boolean;
                 Prompt  : in  String := "Yes") is
    D : Dialog_Type   := Dialog (350, 115, "Input required", Cancel);
    C : Checkbox_Type := Checkbox (D, (15,15), -30, 30, Prompt);
    Y : Button_Type   := Button  (D, (70,55), 80, 25, "OK", OK, True);
    N : Button_Type   := Button  (D, (185,55), 80, 25, "Cancel", Cancel);
  begin
    Ada.Text_IO.Put(Prompt & ' ');
    Set_State (C, Default);
    if Execute(D) = OK then
      Item := Get_State (C);
      Put(Item);
      New_Line;
    else
      raise Input_Cancelled;
    end if;
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Get: get a Boolean value from a file.
  --
  procedure Get (File : in  File_Type;
                 Item : out Boolean) is
    package IO is new Ada.Text_IO.Enumeration_IO (Boolean);
  begin
    IO.Get (File, Item);
  end Get;

  ----------------------------------------------------------------------------
  --
  --  Put: write a Boolean value to the standard output.
  --
  procedure Put (Item : in  Boolean) is
  begin
    Ada.Text_IO.Put(To_String(Item));
  end Put;

  ----------------------------------------------------------------------------
  --
  --  Put: write a Boolean value to the specified file.
  --
  procedure Put (File : in File_Type;
                 Item : in Boolean) is
  begin
    Ada.Text_IO.Put(File,To_String(Item));
  end Put;

  ----------------------------------------------------------------------------
  --
  --  Put_Line: write a Boolean value and newline to the standard output.
  --
  procedure Put_Line (Item : in Boolean) is
  begin
    Put (Item);
    New_Line;
  end Put_Line;

  ----------------------------------------------------------------------------
  --
  --  Put_Line: write a Boolean value and newline to the specified file.
  --
  procedure Put_Line (File : in File_Type;
                      Item : in Boolean) is
  begin
    Put (File, Item);
    New_Line (File);
  end Put_Line;

  ----------------------------------------------------------------------------
  --
  --  To_String: convert a Boolean value to a string.
  --
  function To_String (Item : Boolean) return String is
  begin
    return Boolean'Image(Item);
  end To_String;

  ----------------------------------------------------------------------------
  --
  --  "&": concatenate a string and a Boolean value.
  --
  function "&" (Left  : String;
                Right : Boolean) return String is
  begin
    return Left & To_String(Right);
  end "&";

  ----------------------------------------------------------------------------
  --
  --  "&": concatenate a Boolean value and a string.
  --
  function "&" (Left  : Boolean;
                Right : String) return String is
  begin
    return To_String(Left) & Right;
  end "&";

  ----------------------------------------------------------------------------
  --
  --   G E N E R I C   E N U M E R A T I O N   I N P U T / O U T P U T
  --
  ----------------------------------------------------------------------------

  package body Enumeration_IO is

    --------------------------------------------------------------------------
    --
    --  Get: use a dialog box to get an enumerated value using the supplied
    --       default and echo it to the standard output. The dialog box uses
    --       a combobox filled with a list of the possible values. This is
    --       an internal procedure used to implement the two Get procedures
    --       below.
    --
    procedure Get (Prompt  : in  String;
                   Default : in  Integer;
                   Item    : out Item_Type) is
      D : Dialog_Type   := Dialog (300, 150, "Input required", Cancel);
      P : Label_Type    := Label    (D, (15,15), -30, 30, Prompt);
      C : Combobox_Type := Combobox (D, (15,45), -30);
      Y : Button_Type   := Button   (D, (45,85), 80, 25, "OK", OK, True);
      N : Button_Type   := Button   (D, (155,85), 80, 25, "Cancel", Cancel);
    begin
      -- Fill the combo box with the list of values

      for N in Item_Type'Range loop
        Append_Line (C, Item_Type'Image(N));
      end loop;

      -- Select the default value if there is one (Default < 0 if not)

      if Default >= 0 then
        Select_Line (C, Default+1);
      end if;

      -- Echo the prompt to the standard output and execute the dialog

      Ada.Text_IO.Put(Prompt & ' ');
      if Execute(D) = OK then

        -- OK pressed: get the selection and convert it to the target type,
        -- then echo it to the standard output

        Item := Item_Type'Val(Get_Line(C)-1);
        Put(Item);
        New_Line;

      else

        -- Cancel pressed: raise an exception

        raise Input_Cancelled;

      end if;
    end Get;

    --------------------------------------------------------------------------
    --
    --  Get: use a dialog box to get an enumerated value and echo it to the
    --       standard output.
    --
    procedure Get (Item   : out Item_Type;
                   Prompt : in  String := "Choose a value:") is
    begin
      Get (Prompt, -1, Item);
    end Get;

    --------------------------------------------------------------------------
    --
    --  Get: use a dialog box to get an enumerated value using the supplied
    --       default and echo it to the standard output.
    --
    procedure Get (Item    : out Item_Type;
                   Default : in  Item_Type;
                   Prompt  : in  String := "Choose a value:") is
    begin
      Get (Prompt,
           Item_Type'Pos(Default)-Item_Type'Pos(Item_Type'First),
           Item);
    end Get;

    ----------------------------------------------------------------------------
    --
    --  Get: get an enumerated value from a file.
    --
    procedure Get (File : in  File_Type;
                   Item : out Item_Type) is
      package IO is new Ada.Text_IO.Enumeration_IO (Item_Type);
    begin
      IO.Get (File, Item);
    end Get;

    --------------------------------------------------------------------------
    --
    --  Put: write an enumerated value to the standard output.
    --
    procedure Put (Item : in Item_Type) is
    begin
      Ada.Text_IO.Put(To_String(Item));
    end Put;

    --------------------------------------------------------------------------
    --
    --  Put: write an enumerated value to the specified file.
    --
    procedure Put (File : in File_Type;
                   Item : in Item_Type) is
    begin
      Ada.Text_IO.Put(File,To_String(Item));
    end Put;

    --------------------------------------------------------------------------
    --
    --  Put_Line: write an enumerated value and newline to the standard
    --            output.
    --
    procedure Put_Line (Item : in Item_Type) is
    begin
      Put (Item);
      New_Line;
    end Put_Line;

    --------------------------------------------------------------------------
    --
    --  Put_Line: write an enumerated value and newline to the specified
    --            file.
    --
    procedure Put_Line (File : in File_Type;
                        Item : in Item_Type) is
    begin
      Put (File, Item);
      New_Line (File);
    end Put_Line;

    --------------------------------------------------------------------------
    --
    --  To_String: convert an enumerated value to a string.
    --
    function To_String (Item : Item_Type) return String is
    begin
      return Item_Type'Image(Item);
    end To_String;

    --------------------------------------------------------------------------
    --
    --  "&": concatenate a string and an enumerated value.
    --
    function "&" (Left  : String;
                  Right : Item_Type) return String is
    begin
      return Left & To_String(Right);
    end "&";

    --------------------------------------------------------------------------
    --
    --  "&": concatenate an enumerated value and a string.
    --
    function "&" (Left  : Item_Type;
                  Right : String) return String is
    begin
      return To_String(Left) & Right;
    end "&";

  end Enumeration_IO;

  ----------------------------------------------------------------------------
  --
  --       G E N E R I C   I N T E G E R   I N P U T / O U T P U T
  --
  ----------------------------------------------------------------------------

  package body Integer_IO is

    --------------------------------------------------------------------------
    --
    --  Get: use a dialog box to get a value and echo it to the standard
    --       output.
    --
    procedure Get (Item   : out Item_Type;
                   Prompt : in  String := "Enter an integer:") is
    begin
      loop
        begin
          Item := Item_Type'Value(Get(Prompt,""));
          return;
        exception
          when Constraint_Error =>
            JEWL.IO.Error("You must enter an integer between " &
                              Trim(Item_Type'Image(Item_Type'First)) &
                              " and " &
                              Trim(Item_Type'Image(Item_Type'Last)));
        end;
      end loop;
    end Get;

    --------------------------------------------------------------------------
    --
    --  Get: use a dialog box to get a value using the supplied default
    --       and echo it to the standard output
    --
    procedure Get (Item    : out Item_Type;
                   Default : in  Item_Type;
                   Prompt  : in  String := "Enter an integer:") is
    begin
      loop
        begin
          Item := Item_Type'Value(Get(Prompt,Trim(Item_Type'Image(Default))));
          return;
        exception
          when Constraint_Error =>
            JEWL.IO.Error("You must enter an integer between " &
                              To_String(Item_Type'First) &
                              " and " &
                              To_String(Item_Type'Last));
        end;
      end loop;
    end Get;

    ----------------------------------------------------------------------------
    --
    --  Get: get a value from a file.
    --
    procedure Get (File : in  File_Type;
                   Item : out Item_Type) is
      package IO is new Ada.Text_IO.Integer_IO (Item_Type);
    begin
      IO.Get (File, Item);
    end Get;

    --------------------------------------------------------------------------
    --
    --  Put: write a value to the standard output.
    --
    procedure Put (Item : in Item_Type) is
    begin
      Ada.Text_IO.Put(To_String(Item));
    end Put;

    --------------------------------------------------------------------------
    --
    --  Put: write a value to the specified file.
    --
    procedure Put (File : in File_Type;
                   Item : in Item_Type) is
    begin
      Ada.Text_IO.Put(File,To_String(Item));
    end Put;

    --------------------------------------------------------------------------
    --
    --  Put_Line: write a value and newline to the standard output.
    --
    procedure Put_Line (Item : in Item_Type) is
    begin
      Put (Item);
      New_Line;
    end Put_Line;

    --------------------------------------------------------------------------
    --
    --  Put_Line: write a value and newline to the specified file.
    --
    procedure Put_Line (File : in File_Type;
                        Item : in Item_Type) is
    begin
      Put (File, Item);
      New_Line (File);
    end Put_Line;

    --------------------------------------------------------------------------
    --
    --  To_String: convert a value to a string.
    --
    function To_String (Item : Item_Type) return String is
    begin
      return Trim(Item_Type'Image(Item));
    end To_String;

    --------------------------------------------------------------------------
    --
    --  "&": concatenate a string and a value.
    --
    function "&" (Left  : String;
                  Right : Item_Type) return String is
    begin
      return Left & To_String(Right);
    end "&";

    --------------------------------------------------------------------------
    --
    --  "&": concatenate a value and a string.
    --
    function "&" (Left  : Item_Type;
                  Right : String) return String is
    begin
      return To_String(Left) & Right;
    end "&";

  end Integer_IO;

  ----------------------------------------------------------------------------
  --
  --          G E N E R I C   F L O A T   I N P U T / O U T P U T
  --
  ----------------------------------------------------------------------------

  package body Float_IO is

    --------------------------------------------------------------------------
    --
    --  Get: use a dialog box to get a value and echo it to the standard
    --       output.
    --
    procedure Get (Item   : out Item_Type;
                   Prompt : in  String := "Enter a number:") is
    begin
      loop
        begin
          Item := Item_Type'Value(Get(Prompt,""));
          return;
        exception
          when Constraint_Error =>
            JEWL.IO.Error("You must enter a number between " &
                              To_String(Item_Type'First) &
                              " and " &
                              To_String(Item_Type'Last));
        end;
      end loop;
    end Get;

    --------------------------------------------------------------------------
    --
    --  Get: use a dialog box to get a value using the supplied default
    --       and echo it to the standard output.
    --
    procedure Get (Item    : out Item_Type;
                   Default : in  Item_Type;
                   Prompt  : in  String := "Enter a number:") is
    begin
      loop
        begin
          Item := Item_Type'Value(Get(Prompt,
                                      To_String(Float(Default))));
          return;
        exception
          when Constraint_Error =>
            JEWL.IO.Error("You must enter a number between " &
                              To_String(Float(Item_Type'First)) &
                              " and " &
                              To_String(Float(Item_Type'Last)));
        end;
      end loop;
    end Get;

    ----------------------------------------------------------------------------
    --
    --  Get: get a value from a file.
    --
    procedure Get (File : in  File_Type;
                   Item : out Item_Type) is
      package IO is new Ada.Text_IO.Float_IO (Item_Type);
    begin
      IO.Get (File, Item);
    end Get;

    --------------------------------------------------------------------------
    --
    --  Put: write a value to the standard output.
    --
    procedure Put (Item : in Item_Type) is
    begin
      Ada.Text_IO.Put(To_String(Item));
    end Put;

    --------------------------------------------------------------------------
    --
    --  Put: write a value to the specified file.
    --
    procedure Put (File : in File_Type;
                   Item : in Item_Type) is
    begin
      Ada.Text_IO.Put(File,To_String(Item));
    end Put;

    --------------------------------------------------------------------------
    --
    --  Put_Line: write a value and newline to the standard output.
    --
    procedure Put_Line (Item : in Item_Type) is
    begin
      Put (Item);
      New_Line;
    end Put_Line;

    --------------------------------------------------------------------------
    --
    --  Put_Line: write a value and newline to the specified file.
    --
    procedure Put_Line (File : in File_Type;
                        Item : in Item_Type) is
    begin
      Put (File, Item);
      New_Line (File);
    end Put_Line;

    --------------------------------------------------------------------------
    --
    --  To_String: convert a value to a string.
    --
    function To_String (Item : Item_Type) return String is
      S : String(1..Item_Type'Width);
      N : Integer;
      package Float_IO is new Ada.Text_IO.Float_IO (Item_Type);
    begin
      Float_IO.Put(S,Item,Exp=>0);
      for I in reverse S'Range loop
        N := I;
        exit when S(I) /= '0';
      end loop;
      if S(N) = '.' then
        N := N + 1;
      end if;
      return Trim(S(1..N));
    end To_String;

    --------------------------------------------------------------------------
    --
    --  "&": concatenate a string and a value.
    --
    function "&" (Left  : String;
                  Right : Item_Type) return String is
    begin
      return Left & To_String(Right);
    end "&";

    --------------------------------------------------------------------------
    --
    --  "&": concatenate a value and a string.
    --
    function "&" (Left  : Item_Type;
                  Right : String) return String is
    begin
      return To_String(Left) & Right;
    end "&";

  end Float_IO;

  ----------------------------------------------------------------------------
  --
  --  Open: use a dialog box to get the name of the file to open and
  --        open it for input.
  --
  procedure Open (File  : in out File_Type;
                  Title : in String := "Select input file") is
    D : Open_Dialog_Type := Open_Dialog (Title);
  begin
    if Execute(D) then
      Ada.Text_IO.Put_Line("Opening " & Get_Name(D));
      Ada.Text_IO.Open (File, Mode => Ada.Text_IO.In_File,
                              Name => Get_Name(D));
    else
      raise Input_Cancelled;
    end if;
  end Open;

  ----------------------------------------------------------------------------
  --
  --  Create: use a dialog box to get the name of the file to open and
  --          open it for output.
  --
  procedure Create (File  : in out File_Type;
                    Title : in String := "Select output file") is
    D : Save_Dialog_Type := Save_Dialog (Title,True);
  begin
    if Execute(D) then
      Ada.Text_IO.Put_Line("Creating " & Get_Name(D));
      Ada.Text_IO.Create (File, Name => Get_Name(D));
    else
      raise Input_Cancelled;
    end if;
  end Create;

  ----------------------------------------------------------------------------
  --
  --  Append: use a dialog box to get the name of the file to open and
  --          open it for appending.
  --
  procedure Append (File  : in out File_Type;
                    Title : in String := "Select output file") is
    D : Save_Dialog_Type := Save_Dialog (Title, False);
  begin
    if Execute(D) then
      Ada.Text_IO.Put_Line("Appending to " & Get_Name(D));
      begin
        Ada.Text_IO.Open (File, Mode => Ada.Text_IO.Append_File,
                                Name => Get_Name(D));
      exception
        when Ada.Text_IO.Name_Error =>
          Ada.Text_IO.Create (File, Name => Get_Name(D));
      end;
    else
      raise Input_Cancelled;
    end if;
  end Append;

end JEWL.IO;
------------------------------------------------------------------------------
--                                                                          --
--                             J E W L . I O                                --
--                                                                          --
--   An extended input-output package for beginners, using graphical        --
--   dialogs for input which also write log information to the standard     --
--   output.                                                                --
--                                                                          --
--   The documentation below assumes that you are familiar with the main    --
--   features of the input and output facilities provide by Ada.Text_IO.    --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-io.ads 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-io.ads $
-- Revision 1.7  2007/01/08 17:00:00  JE
-- * Fixed linker options in JEWL.Win32_Interface to accommodate changes to GNAT
--   GPL 2006 compiler (thanks to John McCormick for this)
-- * Added delay in message loop to avoid the appearance of hogging 100% of CPU
--   time
--
-- Revision 1.6  2001/11/02 16:00:00  JE
-- * Fixed canvas bug when saving an empty canvas
-- * Restore with no prior save now acts as erase
-- * Removed redundant variable declaration in Image function
--
-- Revision 1.5  2001/08/22 15:00:00  JE
-- * Minor bugfix to Get_Text for combo boxes
-- * Minor changes to documentation (including new example involving dialogs)
--
-- Revision 1.4  2001/01/25 09:00:00  je
-- Changes visible to the user:
--
-- * Added support for drawing bitmaps on canvases (Draw_Image operations
--   and new type Image_Type)
-- * Added Play_Sound
-- * Added several new operations on all windows: Get_Origin, Get_Width,
--   Get_Height, Set_Origin, Set_Size and Focus
-- * Added several functions giving screen and window dimensions: Screen_Width,
--   Screen_Height, Frame_Width, Frame_Height, Dialog_Width, Dialog_Height and
--   Menu_Height
-- * Canvases can now handle keyboard events: new constructor and Key_Code added
-- * Added procedure Play_Sound
-- * Operations "+" and "-" added for Point_Type
-- * Pens can now be zero pixels wide
-- * The absolute origin of a frame can now have be specified when the frame
--   is created
-- * Added new File_Dialog operations Add_Filter and Set_Directory
-- * Added Get_Line renames to JEWL.IO for compatibility with Ada.Text_IO
-- * Added all the Get(File,Item) operations mentioned in documentation but
--   unaccountably missing :-(
-- * Documentation updated to reflect the above changes
-- * HTML versions of public package specifications added with links from
--   main documentation pages
--
-- Other internal changes:
--
-- * Canvas fonts, pens etc. now use JEWL.Reference_Counted_Type rather than
--   reinventing this particular wheel, as do images
-- * Various minor code formatting changes: some code reordered for clarity,
--   some comments added or amended,
-- * Changes introduced in 1.2 to support GNAT 3.10 have been reversed, since
--   GNAT 3.10 still couldn't compile this code correctly... ;-(
--
-- Outstanding issues:
--
-- * Optimisation breaks the code (workaround: don't optimise)
--
-- Revision 1.3  2000/07/07 12:00:00  je
-- * JEWL.Simple_Windows added; JEWL.IO modified to use JEWL.Simple_Windows.
-- * JEWL.IO bug fix: Put_Line to file wrote newline to standard output
--   instead of to the file (thanks to Jeff Carter for pointing this out).
-- * Panels fixed so that mouse clicks are passed on correctly to subwindows.
-- * Memos fixed so that tabs are handled properly.
-- * Password feature added to editboxes.
-- * Minor typos fixed in comments within the package sources.
-- * Documentation corrected and updated following comments from Moti Ben-Ari
--   and Don Overheu.
--
-- Revision 1.2  2000/04/18 20:00:00  je
-- * Minor code changes to enable compilation by GNAT 3.10
-- * Minor documentation errors corrected
-- * Some redundant "with" clauses removed
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

with Ada.Text_IO;

package JEWL.IO is

  ----------------------------------------------------------------------------
  --
  -- Some local names for types from Ada.Text_IO
  --
  subtype Positive_Count is Ada.Text_IO.Positive_Count;
  subtype File_Type      is Ada.Text_IO.File_Type;

  ----------------------------------------------------------------------------
  --
  -- Exceptions that this package might raise
  --
  Input_Cancelled : exception;      -- the user cancelled an input dialog

  ----------------------------------------------------------------------------
  --
  -- Routines to display message boxes
  --
  procedure Error   (Text : in String);                 -- an error message
  function  Query   (Text : in String) return Boolean;  -- a yes/no query
  procedure Message (Text : in String);                 -- an information message

  ----------------------------------------------------------------------------
  --
  -- Output a newline, as in Ada.Text_IO
  --
  procedure New_Line (Spacing : in Ada.Text_IO.Positive_Count := 1)
            renames Ada.Text_IO.New_Line;

  ----------------------------------------------------------------------------
  --
  -- Opening and closing files, using standard file dialogs to get filenames
  --
  procedure Open   (File  : in out File_Type;   -- open an existing file
                    Title : in String := "Select input file");
  procedure Create (File  : in out File_Type;   -- create a new file
                    Title : in String := "Select output file");
  procedure Append (File  : in out File_Type;   -- append to existing/new file
                    Title : in String := "Select output file");
  procedure Close  (File   : in out Ada.Text_IO.File_Type)
                                renames Ada.Text_IO.Close;
                                                -- close an open file

  ----------------------------------------------------------------------------
  --
  --  Standard file positioning operations and queries, as in Ada.Text_IO
  --
  procedure New_Line    (File    : in Ada.Text_IO.File_Type;
                         Spacing : in Positive_Count := 1)
                                renames Ada.Text_IO.New_Line;
  procedure Skip_Line   (File    : in Ada.Text_IO.File_Type;
                         Spacing : in Positive_Count := 1)
                                renames Ada.Text_IO.Skip_Line;
  function  End_Of_Line (File    : in Ada.Text_IO.File_Type)
                         return Boolean
                                renames Ada.Text_IO.End_Of_Line;
  function  End_Of_File (File    : in Ada.Text_IO.File_Type)
                         return Boolean
                                renames Ada.Text_IO.End_Of_File;

  ----------------------------------------------------------------------------
  --
  --      P R I M A R Y   I N P U T / O U T P U T   R O U T I N E S
  --
  --                         ---- Input ----
  --
  --  For each scalar type (with generic packages for enumerations, integral
  --  types and floating point types, and support provided as standard for
  --  String, Character, Integer, Float, and Boolean):
  --
  --  Get (Item    => X,       -- get a value into X,
  --       Default => Y,       -- with an initial default value of Y (optional)
  --       Prompt  => Z);      -- displaying Z as a prompt (optional),
  --
  --  Get (File    => F,
  --       Item    => X);      -- get a value into X from file F
  --
  --  The prompt is always a string; the default value depends on the type of
  --  data involved. In the case of String there is a potential ambiguity as
  --  the prompt and default values are both strings. It is recommended that
  --  the Prompt and Default parameters are always specified as "Prompt=>X"
  --  and "Default=>X" to avoid confusion.
  --
  --  Strings are a bit different: there is a function which gets a String
  --  (unconstrained) and a procedure which gets a value into a String but
  --  which also returns the length of the string in Length:
  --
  --  S := Get (Prompt  => Y,  -- optional
  --            Default => Z); -- optional
  --
  --  Get (Item    => X,       -- get a value into X,
  --       Length  => L,       -- whose actual length is L,
  --       Prompt  => Y,       -- displaying Y as a prompt (optional),
  --       Default => Z);      -- with an initial default value of Z (optional)
  --
  --  Get (File    => F,
  --       Item    => X,       -- get a value into X from file F,
  --       Length  => L);      -- whose actual length is L
  --
  --                        ---- Output ----
  --
  --  Values can be output to the standard output or a file, with or without
  --  a following newline:
  --
  --  Put (Item => X);         -- write the value of X on the standard output
  --  Put (File => F,
  --       Item => X);         -- write the value of X on the file F
  --
  --  Put_Line (Item => X);    -- write X and a newline on the standard output
  --  Put_Line (File => F,
  --            Item => X);    -- write X and a newline on the file F
  --
  --                    ---- Type conversion ----
  --
  --  S := To_String(X)        -- convert a (non-string) type to a String
  --  S := S & X;              -- concatenate String and X
  --  S := X & S;              -- concatenate X and String
  --
  ----------------------------------------------------------------------------
  --
  --  String and Character input
  --
  function  Get (Prompt  : in  String := "Enter your text:";
                 Default : in  String := "")
                 return String;     -- display a dialog with a label (Prompt)
                                    -- and an editbox (initial value Default)
                                    -- and return the contents of the editbox
                                    -- as a String (unconstrained)

  procedure Get (Item    : out String;
                 Length  : out Natural;
                 Prompt  : in  String := "Enter your text:";
                 Default : in  String := "");
                                    -- use the same edit dialog to get a string
                                    -- into a variable, the maximum length being
                                    -- limited by the size of Item and the actual
                                    -- length being stored in Length, with an
                                    -- initial default value
  procedure Get (File    : in Ada.Text_IO.File_Type;
                 Item    : out Character)
                                renames Ada.Text_IO.Get;
                                    -- get a character from a file, from Text_IO
  procedure Get (File    : in Ada.Text_IO.File_Type;
                 Item    : out String;
                 Length  : out Natural)
                                renames Ada.Text_IO.Get_Line;
                                    -- get a string and its length from a file,
                                    -- from Text_IO

  ----------------------------------------------------------------------------
  --  Renamings for consistency with Ada.Text_IO
  ----------------------------------------------------------------------------

  procedure Get_Line (Item    : out String;
                      Length  : out Natural     ;
                      Prompt  : in  String := "Enter your text:";
                      Default : in  String := "")  renames Get;

  procedure Get_Line (File    : in Ada.Text_IO.File_Type;
                      Item    : out String;
                      Length  : out Natural)       renames Get;

  ----------------------------------------------------------------------------
  --
  --  String and Character output
  --
  procedure Put (Item    : in String)
                                renames Ada.Text_IO.Put;
                                    -- output a string
  procedure Put (Item    : in Character)
                                renames Ada.Text_IO.Put;
                                    -- output a character
  procedure Put (File    : in Ada.Text_IO.File_Type;
                 Item    : in Character)
                                renames Ada.Text_IO.Put;
                                    -- output a character to a file
  procedure Put (File    : in Ada.Text_IO.File_Type;
                 Item    : in String)
                                renames Ada.Text_IO.Put;
                                    -- output a string to a file

  ----------------------------------------------------------------------------
  --
  --  String and Character output, with newlines
  --
  procedure Put_Line (Item : in String)
                                renames Ada.Text_IO.Put_Line;
                                    -- output a string and newline
  procedure Put_Line (Item : in Character);
                                    -- output a character and newline
  procedure Put_Line (File : in Ada.Text_IO.File_Type;
                      Item : in String)
                                renames Ada.Text_IO.Put_Line;
                                    -- output a string and newline to a file
  procedure Put_Line (File : in File_Type;
                      Item : in Character);
                                    -- output a character and newline to a file

  ----------------------------------------------------------------------------
  --
  --               INTEGER INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------
  --
  --  Integer input
  --
  procedure Get (Item    : out Integer;
                 Prompt  : in  String := "Enter an integer:");
                                    -- display a dialog with a label (Prompt)
                                    -- and a blank editbox which returns the
                                    -- contents of the editbox as an Integer
  procedure Get (Item    : out Integer;
                 Default : in  Integer;
                 Prompt  : in  String := "Enter an integer:");
                                    -- display the same dialog with the editbox
                                    -- initialised to the value of Default
  procedure Get (File    : in  File_Type;
                 Item    : out Integer);
                                    -- read an Integer from a file

  ----------------------------------------------------------------------------
  --
  --  Integer output
  --
  procedure Put (Item    : in Integer);
                                    -- output an Integer
  procedure Put (File    : in File_Type;
                 Item    : in Integer);
                                    -- output an Integer to a file

  ----------------------------------------------------------------------------
  --
  --  Integer output, with newlines
  --
  procedure Put_Line (Item : in Integer);
                                    -- output an Integer and a newline
  procedure Put_Line (File : in File_Type;
                      Item : in Integer);
                                    -- output an Integer and a newline to a file

  ----------------------------------------------------------------------------
  --
  --  Integer conversion routines
  --
  function  To_String (Item : Integer) return String;
                                    -- convert an integer to a string

  function  "&" (Left    : String;
                 Right   : Integer)    return String;
                                    -- concatenate String & Integer
  function  "&" (Left    : Integer;
                 Right   : String)     return String;
                                    -- concatenate Integer & String

  ----------------------------------------------------------------------------
  --
  --                FLOAT INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------
  --
  --  Float input
  --
  procedure Get (Item    : out Float;
                 Prompt  : in  String := "Enter a number:");
                                    -- display a dialog with a label (Prompt)
                                    -- and a blank editbox which returns the
                                    -- contents of the editbox as an Integer
  procedure Get (Item    : out Float;
                 Default : in  Float;
                 Prompt  : in  String := "Enter a number:");
                                    -- display the same dialog with the editbox
                                    -- initialised to the value of Default
  procedure Get (File    : in  File_Type;
                 Item    : out Float);
                                    -- read a Float from a file

  ----------------------------------------------------------------------------
  --
  --  Float output
  --
  procedure Put (Item    : in  Float);
  procedure Put (File    : in  File_Type;
                 Item    : in  Float);

  ----------------------------------------------------------------------------
  --
  --  Float output, with newlines
  --
  procedure Put_Line (Item : in Float);
  procedure Put_Line (File : in File_Type;
                      Item : in Float);

  ----------------------------------------------------------------------------
  --
  --   Float conversion routines
  --
  function  To_String (Item : Float) return String;

  function  "&" (Left    : String;
                 Right   : Float)    return String;
  function  "&" (Left    : Float;
                 Right   : String)   return String;

  ----------------------------------------------------------------------------
  --
  --               BOOLEAN INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------
  --
  --  Boolean input
  --
  procedure Get (Item    : out Boolean;
                 Prompt  : in  String := "Yes or no?");
                                    -- display a dialog with Yes/No/Cancel
                                    -- buttons
  procedure Get (Item    : out Boolean;
                 Default : in  Boolean;
                 Prompt  : in  String := "Yes");
                                    -- display a dialog with a checkbox
                                    -- initialised as specified by Default
  procedure Get (File    : in  File_Type;
                 Item    : out Boolean);
                                    -- read a Boolean from a file

  ----------------------------------------------------------------------------
  --
  --  Boolean output
  --
  procedure Put (Item    : in Boolean);
  procedure Put (File    : in File_Type;
                 Item    : in Boolean);

  ----------------------------------------------------------------------------
  --
  --  Boolean output, with newlines
  --
  procedure Put_Line (Item : in Boolean);
  procedure Put_Line (File : in File_Type;
                      Item : in Boolean);

  ----------------------------------------------------------------------------
  --
  --  Boolean conversion routines
  --
  function  To_String (Item : Boolean) return String;

  function  "&" (Left    : String;
                 Right   : Boolean)    return String;
  function  "&" (Left    : Boolean;
                 Right   : String)     return String;

  ----------------------------------------------------------------------------
  --
  --             ENUMERATION INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------

  generic
    type Item_Type is (<>);
  package Enumeration_IO is

    --------------------------------------------------------------------------
    --
    --  Enumeration input
    --
    procedure Get (Item    : out Item_Type;
                   Prompt  : in  String := "Choose a value:");
                                    -- display a dialog with a label (Prompt)
                                    -- and a combobox listing all values of
                                    -- type Item_Type
    procedure Get (Item    : out Item_Type;
                   Default : in  Item_Type;
                   Prompt  : in  String := "Choose a value:");
                                    -- display the same dialog with the combobox
                                    -- initialised to Default
    procedure Get (File    : in  File_Type;
                   Item    : out Item_Type);
                                    -- read an Item_Type value from a file

    --------------------------------------------------------------------------
    --
    --  Enumeration output
    --
    procedure Put (Item    : in  Item_Type);
    procedure Put (File    : in File_Type;
                   Item    : in Item_Type);

    --------------------------------------------------------------------------
    --
    --  Enumeration output, with newlines
    --
    procedure Put_Line (Item : in Item_Type);
    procedure Put_Line (File : in File_Type;
                        Item : in Item_Type);

    --------------------------------------------------------------------------
    --
    --  Enumeration conversion routines
    --
    function  To_String (Item : Item_Type) return String;

    function  "&" (Left    : String;
                   Right   : Item_Type)    return String;
    function  "&" (Left    : Item_Type;
                   Right   : String)       return String;

  end Enumeration_IO;

  ----------------------------------------------------------------------------
  --
  --         GENERIC INTEGRAL INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------

  generic
    type Item_Type is range <>;
  package Integer_IO is

    --------------------------------------------------------------------------
    --
    --  Generic integral input
    --
    procedure Get (Item    : out Item_Type;
                   Prompt  : in  String := "Enter an integer:");
                                    -- display a dialog with a label (Prompt)
                                    -- and a blank editbox which returns the
                                    -- contents of the editbox as an Item_Type
                                    -- integral value
    procedure Get (Item    : out Item_Type;
                   Default : in  Item_Type;
                   Prompt  : in  String := "Enter an integer:");
                                    -- display the same dialog with the editbox
                                    -- initialised to the value of Default
    procedure Get (File    : in  File_Type;
                   Item    : out Item_Type);
                                    -- read an Item_Type value from a file

    --------------------------------------------------------------------------
    --
    --  Generic integral output
    --
    procedure Put (Item    : in  Item_Type);
    procedure Put (File    : in File_Type;
                   Item    : in Item_Type);

    --------------------------------------------------------------------------
    --
    --  Generic integral output, with newlines
    --
    procedure Put_Line (Item : in Item_Type);
    procedure Put_Line (File : in File_Type;
                        Item : in Item_Type);

    --------------------------------------------------------------------------
    --
    -- Generic integral conversion routines
    --
    function  To_String (Item : Item_Type) return String;

    function  "&" (Left    : String;
                   Right   : Item_Type)    return String;
    function  "&" (Left    : Item_Type;
                   Right   : String)       return String;

  end Integer_IO;

  ----------------------------------------------------------------------------
  --
  --       GENERIC FLOATING-POINT INPUT, OUTPUT AND CONVERSION
  --
  ----------------------------------------------------------------------------

  generic
    type Item_Type is digits <>;
  package Float_IO is

    --------------------------------------------------------------------------
    --
    --  Generic floating-point input
    --
    procedure Get (Item    : out Item_Type;
                   Prompt  : in  String := "Enter a number:");
                                    -- display a dialog with a label (Prompt)
                                    -- and a blank editbox which returns the
                                    -- contents of the editbox as an Item_Type
                                    -- floating-point value
    procedure Get (Item    : out Item_Type;
                   Default : in  Item_Type;
                   Prompt  : in  String := "Enter a number:");
                                    -- display the same dialog with the editbox
                                    -- initialised to the value of Default
    procedure Get (File    : in  File_Type;
                   Item    : out Item_Type);
                                    -- read an Item_Type value from a file

    --------------------------------------------------------------------------
    --
    --  Generic floating-point output
    --
    procedure Put (Item    : in  Item_Type);
    procedure Put (File    : in File_Type;
                   Item    : in Item_Type);

    --------------------------------------------------------------------------
    --
    --  Generic floating-point output, with newlines
    --
    procedure Put_Line (Item : in Item_Type);
    procedure Put_Line (File : in File_Type;
                        Item : in Item_Type);

    --------------------------------------------------------------------------
    --
    --  Generic floating-point conversion routines
    --
    function  To_String (Item : Item_Type) return String;

    function  "&" (Left    : String;
                   Right   : Item_Type)    return String;
    function  "&" (Left    : Item_Type;
                   Right   : String)       return String;

  end Float_IO;

end JEWL.IO;
------------------------------------------------------------------------------
--                                                                          --
--                J E W L . M E S S A G E _ H A N D L I N G                 --
--                                                                          --
--   The body of a private package which defines the message-handling task  --
--   required by JEWL.Windows, the protected record used to communicate     --
--   with it, and related operations.                                       --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-message_handling.adb 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-message_handling.adb $
-- Revision 1.7  2007/01/08 17:00:00  JE
-- * Fixed linker options in JEWL.Win32_Interface to accommodate changes to GNAT
--   GPL 2006 compiler (thanks to John McCormick for this)
-- * Added delay in message loop to avoid the appearance of hogging 100% of CPU
--   time
--
-- Revision 1.6  2001/11/02 16:00:00  JE
-- * Fixed canvas bug when saving an empty canvas
-- * Restore with no prior save now acts as erase
-- * Removed redundant variable declaration in Image function
--
-- Revision 1.5  2001/08/22 15:00:00  JE
-- * Minor bugfix to Get_Text for combo boxes
-- * Minor changes to documentation (including new example involving dialogs)
--
-- Revision 1.4  2001/01/25 09:00:00  je
-- Changes visible to the user:
--
-- * Added support for drawing bitmaps on canvases (Draw_Image operations
--   and new type Image_Type)
-- * Added Play_Sound
-- * Added several new operations on all windows: Get_Origin, Get_Width,
--   Get_Height, Set_Origin, Set_Size and Focus
-- * Added several functions giving screen and window dimensions: Screen_Width,
--   Screen_Height, Frame_Width, Frame_Height, Dialog_Width, Dialog_Height and
--   Menu_Height
-- * Canvases can now handle keyboard events: new constructor and Key_Code added
-- * Added procedure Play_Sound
-- * Operations "+" and "-" added for Point_Type
-- * Pens can now be zero pixels wide
-- * The absolute origin of a frame can now have be specified when the frame
--   is created
-- * Added new File_Dialog operations Add_Filter and Set_Directory
-- * Added Get_Line renames to JEWL.IO for compatibility with Ada.Text_IO
-- * Added all the Get(File,Item) operations mentioned in documentation but
--   unaccountably missing :-(
-- * Documentation updated to reflect the above changes
-- * HTML versions of public package specifications added with links from
--   main documentation pages
--
-- Other internal changes:
--
-- * Canvas fonts, pens etc. now use JEWL.Reference_Counted_Type rather than
--   reinventing this particular wheel, as do images
-- * Various minor code formatting changes: some code reordered for clarity,
--   some comments added or amended,
-- * Changes introduced in 1.2 to support GNAT 3.10 have been reversed, since
--   GNAT 3.10 still couldn't compile this code correctly... ;-(
--
-- Outstanding issues:
--
-- * Optimisation breaks the code (workaround: don't optimise)
--
-- Revision 1.3  2000/07/07 12:00:00  je
-- * JEWL.Simple_Windows added; JEWL.IO modified to use JEWL.Simple_Windows.
-- * JEWL.IO bug fix: Put_Line to file wrote newline to standard output
--   instead of to the file (thanks to Jeff Carter for pointing this out).
-- * Panels fixed so that mouse clicks are passed on correctly to subwindows.
-- * Memos fixed so that tabs are handled properly.
-- * Password feature added to editboxes.
-- * Minor typos fixed in comments within the package sources.
-- * Documentation corrected and updated following comments from Moti Ben-Ari
--   and Don Overheu.
--
-- Revision 1.2  2000/04/18 20:00:00  je
-- * Minor code changes to enable compilation by GNAT 3.10
-- * Minor documentation errors corrected
-- * Some redundant "with" clauses removed
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

package body JEWL.Message_Handling is

  use JEWL.Window_Implementation;
  use JEWL.Win32_Interface;
  use Ada.Exceptions;

  use type System.Address;
  use type Win32_BOOL, Win32_DWORD, Win32_LONG, Win32_UINT;

  -----------------------------------------------------------------------------
  --  Type conversions (needed below)
  -----------------------------------------------------------------------------

  function To_Window_Ptr is new Ada.Unchecked_Conversion
                                            (Win32_LONG,Window_Ptr);
  function To_LONG       is new Ada.Unchecked_Conversion
                                            (Window_Ptr,Win32_LONG);

  ----------------------------------------------------------------------------
  --
  --                        W I N D O W _ I N F O
  --
  ----------------------------------------------------------------------------

  protected body Window_Info is

    --------------------------------------------------------------------------
    --
    --  Get_Command: block until a command is available (or the message loop
    --               has failed) and then return it. The command is also reset.
    --               Program_Error is raised if the message loop has failed.
    --
    entry Get_Command (Cmd : out Natural)
                       when Command /= 0 or Task_Failed is
    begin
      if Task_Failed then
        Raise_Exception (Program_Error'Identity,
                         "caused by " & Exception_Name(Failure_Info) &
                         ": " & Exception_Message(Failure_Info));
      end if;
      Cmd := Command - WM_USER;
      Command := 0;
    end Get_Command;

    --------------------------------------------------------------------------
    --
    --  Test_Command: test if a command is pending. Program_Error is raised
    --                if the message loop task has failed.
    --
    function Test_Command return Boolean is
    begin
      if Task_Failed then
        Raise_Exception (Program_Error'Identity,
                         "caused by " & Exception_Name(Failure_Info) &
                         ": " & Exception_Message(Failure_Info));
      end if;
      return Command /= 0;
    end Test_Command;

    --------------------------------------------------------------------------
    --
    --  Set_Command: store the code for an available command. This is a
    --               procedure, not an entry, so the the message loop won't
    --               stall. If commands aren't handled in time, they'll be
    --               overwritten by the next one that comes along.
    --
    procedure Set_Command (Cmd : in Natural) is
    begin
      Command := Cmd;
    end Set_Command;

    --------------------------------------------------------------------------
    --
    --  Get_Dialog: swap the handle of the current active window with the
    --              parameter (i.e. record the new handle and return the
    --              old one).
    --
    procedure Get_Dialog (Dlg : in out Win32_HWND) is
      D : Win32_HWND := Dialog;
    begin
      Dialog := Dlg;
      Dlg := D;
    end Get_Dialog;

    --------------------------------------------------------------------------
    --
    --  Active_Window: get the handle of the current active window.
    --
    function Active_Window return Win32_HWND is
    begin
      return Dialog;
    end Active_Window;

    --------------------------------------------------------------------------
    --
    --  Record_Error : record the occurrence of an exception which aborted
    --                 the message loop task.
    --
    procedure Record_Error (Err : in Ada.Exceptions.Exception_Occurrence) is
    begin
      Task_Failed := True;
      Ada.Exceptions.Save_Occurrence (Failure_Info, Err);
    end Record_Error;

  end Window_Info;

  ----------------------------------------------------------------------------
  --
  --                  U T I L I T Y   F U N C T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Get_Actual_Bounds: check if any bounds supplied are non-positive
  --                     (i.e. relative to parent size) and recalculate
  --                     the bounds if so. Resize is set True if any
  --                     bounds have been updated.
  --
  procedure Get_Actual_Bounds (Parent : in Win32_HWND;
                               Top    : in out Integer;
                               Left   : in out Integer;
                               Width  : in out Integer;
                               Height : in out Integer;
                               Resize : out Boolean) is
    R : aliased Win32_RECT;
  begin
    -- Do any bounds need recalculationg?

    Resize := Top < 0 or Left < 0 or Width <= 0 or Height <= 0;

    if Resize then
      -- Top-level windows take their bounds from the screen, child windows
      -- take them from their parent

      if Parent = System.Null_Address then
        R := (0, 0, Win32_LONG(GetSystemMetrics(SM_CXSCREEN)),
                    Win32_LONG(GetSystemMetrics(SM_CYSCREEN)));
      else
        Bool_Dummy := GetClientRect (Parent, R'Unchecked_Access);
      end if;

      -- Recalculate left side relative to parent bounds if necessary

      if Left < 0 then
        Left := Left + Integer(R.Right);
      end if;

      -- Recalculate top relative to parent bounds if necessary

      if Top < 0 then
        Top := Top + Integer(R.Bottom);
      end if;

      -- Recalculate width relative to parent bounds if necessary

      if Width <= 0 then
        Width := Width + Integer(R.Right);
      end if;

      -- Recalculate height relative to parent bounds if necessary

      if Height <= 0 then
        Height := Height + Integer(R.Bottom);
      end if;

    end if;

  end Get_Actual_Bounds;

  ----------------------------------------------------------------------------
  --
  --  Build_Window: construct a top-level window of the specified class,
  --                with extended and normal styles as specified, and
  --                make the window visible or invisible as requested.
  --
  procedure Build_Window (Window : in Main_Window_Ptr;
                          Class  : in Win32_String;
                          Title  : in Win32_String;
                          XStyle : in Win32_DWORD;
                          Style  : in Win32_DWORD;
                          Show   : in Boolean) is
    S : Win32_DWORD := Style;
  begin
    -- Add the VISIBLE style if the window is to be shown

    if Show then
      S := S or WS_VISIBLE;
    end if;

    -- Create the window using the supplied parameters

    Window.Handle := CreateWindowEx (XStyle,
                                     To_LPCSTR(Class),
                                     To_LPCSTR(Title),
                                     S,
                                     Win32_INT(Window.Left),
                                     Win32_INT(Window.Top),
                                     Win32_INT(Window.Width),
                                     Win32_INT(Window.Height),
                                     System.Null_Address,
                                     System.Null_Address,
                                     Get_hInstance,
                                     Window.all'Address);

    -- Store a pointer to the Window_Internals in the user data area
    -- so that the message loop can find it

    Long_Dummy := SendMessage (Window.Handle, WM_SETFONT,
                               To_WPARAM(Window.Font),0);

    -- Show or hide the window as requested

    if Show then
      Bool_Dummy := ShowWindow (Window.Handle, SW_SHOWNORMAL);
    else
      Bool_Dummy := ShowWindow (Window.Handle, SW_HIDE);
    end if;

    -- Mark the window for repainting

    Bool_Dummy := UpdateWindow (Window.Handle);

  end Build_Window;

  ----------------------------------------------------------------------------
  --
  --  Build_Child: build a child window attached to the specified parent
  --               belonging to the specified class, using the specified
  --               title (caption text) as appropriate. Use the specified
  --               extended styles and normal styles, and set the window
  --               dimensions (relative to the parent's client area).
  --
  procedure Build_Child (Window : in Window_Ptr;
                         Parent : in Container_Ptr;
                         Class  : in Win32_String;
                         Title  : in Win32_String;
                         XStyle : in Win32_DWORD;
                         Style  : in Win32_DWORD;
                         Top    : in Integer;
                         Left   : in Integer;
                         Width  : in Integer;
                         Height : in Integer) is
    W : Window_Ptr := Window;
    H : Win32_HWND;
    S : Win32_DWORD := Style;
  begin
    -- The parent window's Group flag determines whether the WS_GROUP style
    -- should be applied to the child control in case it isn't already set.

    if Parent.Group then
      S := S or WS_GROUP;
    end if;

    -- All child windows except radiobuttons have WS_GROUP set anyway; the
    -- first radiobutton in a group also needs it set, so record if it was
    -- already set in the requested style to ensure that it will be applied
    -- to the next control regardless.

    Parent.Group := (Style and WS_GROUP) /= 0;

    -- Create the window using the supplied parameters

    Window.Handle := CreateWindowEx
                        (XStyle,
                         To_LPCSTR(Class), To_LPCSTR(Title),
                         S or WS_CHILD or WS_VISIBLE,
                         Win32_INT(Left), Win32_INT(Top),
                         Win32_INT(Width), Win32_INT(Height),
                         Parent.Handle, To_Handle(Window.Action),
                         Get_hInstance,
                         System.Null_Address);

    -- Store a pointer to the Window_Internals in the user data area
    -- so that the message loop can find it

    Long_Dummy := SetWindowLong (Window.Handle,
                                 GWL_USERDATA, To_LONG(Window));

    -- Scan the parent chain until a font is found if there isn't one
    -- for this window (i.e. Parent_Font has been used)

    while W.Font = System.Null_Address and W.Parent /= null loop
      W := Window_Ptr(W.Parent);
    end loop;

    -- If a font was found, select it

    if W.Font /= System.Null_Address then
      Long_Dummy := SendMessage (Window.Handle, WM_SETFONT,
                                 To_WPARAM(W.Font),0);
    end if;

    -- If this is a tabstop control (i.e. the tab key will activate it),
    -- find the top-level parent and check if it's the first tabstop in
    -- the window. If it is, give it the keyboard focus. The WM_ACTIVATE
    -- message handler will track it once it's been set. Don't explicitly
    -- set the focus unless the control is visible.

    if (Style and WS_TABSTOP) /= 0 then
      while W.Parent /= null loop
        W := Window_Ptr(W.Parent);              -- climb the parent chain
      end loop;
      if Main_Window_Ptr(W).Focus = System.Null_Address then

        -- At this point, we know that this is the first tabstop control
        -- to be attached to the top-level window W. Record the fact, and
        -- focus on this window if it's visible.

        Main_Window_Ptr(W).Focus := Window.Handle;
        if IsWindowVisible(W.Handle) /= 0 then
          H := SetFocus (Window.Handle);        -- set focus if visible
        end if;
      end if;
    end if;

  end Build_Child;

  ----------------------------------------------------------------------------
  --
  --                       M E S S A G E _ L O O P
  --
  --  This task drives the Windows message loop, and starts as soon as the
  --  package body is elaborated. It uses a global variable Frames to keep
  --  track of the number of top-level windows to allow it to terminate if
  --  there are no top-level windows open. The repetition of entry handlers
  --  is required because we need a terminate alternative (which rules out
  --  having an else part) and we also need to handle Windows messages (which
  --  must be done by the task that creates the windows, i.e. this one) which
  --  can only be done by checking for messages in an else part.
  --
  ----------------------------------------------------------------------------

  Frames : Integer := 0;    -- a global variable used by the message loop
                            -- to record the number of top-level windows.
  task body Message_Loop is
    M : aliased Win32_MSG;
    H : Win32_HWND;
  begin
    loop

      -- The following select statement is executed when there are no
      -- top level windows. The alternatives are to create a window,
      -- destroy a window, show a common dialog or to terminate. The
      -- destroy alternative is needed because the main window cleanup
      -- operation will call it if the window handle is still valid
      -- when the internal window structure is being finalized.

      select
        accept Create_Window (Window : in Main_Window_Ptr;
                              Class  : in Win32_String;
                              Title  : in Win32_String;
                              XStyle : in Win32_DWORD;
                              Style  : in Win32_DWORD;
                              Show   : in Boolean)
        do
          Build_Window (Window, Class, Title, XStyle, Style, Show);
          Frames := Frames + 1;
        end Create_Window;
      or
        accept Destroy_Window (Handle : in Win32_HWND)
        do
          Bool_Dummy := DestroyWindow (Handle);
        end Destroy_Window;
      or
        accept Show_Dialog (Dialog : in Common_Dialog_Ptr;
                            Result : out Boolean)
        do
          Result := Show_Dialog(Dialog);
        end Show_Dialog;
      or
        terminate;
      end select;

      -- Once the first top-level window has been created, this inner loop
      -- will accept requests to create top-level or child windows, to show
      -- common dialogs or to destroy windows. When there are no pending
      -- requests it will look to see if there is a pending Windows message
      -- and process it  if so. The loop ends when the last top-level window
      -- is destroyed, which then takes us back to the top of the outer loop
      -- to wait for a top-level window to be created or destroyed, or a
      -- termination request.

      while Frames > 0 loop
        select
          accept Create_Child (Window : in Window_Ptr;
                               Parent : in Container_Ptr;
                               Class  : in Win32_String;
                               Title  : in Win32_String;
                               XStyle : in Win32_DWORD;
                               Style  : in Win32_DWORD;
                               Top    : in Integer;
                               Left   : in Integer;
                               Width  : in Integer;
                               Height : in Integer)
          do
            Build_Child (Window, Parent, Class, Title, XStyle, Style,
                         Top, Left, Width, Height);
          end Create_Child;
        or
          accept Create_Window (Window : in Main_Window_Ptr;
                                Class  : in Win32_String;
                                Title  : in Win32_String;
                                XStyle : in Win32_DWORD;
                                Style  : in Win32_DWORD;
                                Show   : in Boolean)
          do
            Build_Window (Window, Class, Title, XStyle, Style, Show);
            Frames := Frames + 1;
          end Create_Window;
        or
          accept Set_Focus (Window : in Win32_HWND)
          do
            Long_Dummy := To_LONG (SetFocus(Window));
          end Set_Focus;
        or
          accept Destroy_Window (Handle : in Win32_HWND)
          do
            Bool_Dummy := DestroyWindow (Handle);
          end Destroy_Window;
        or
          accept Show_Dialog (Dialog : in Common_Dialog_Ptr;
                              Result : out Boolean)
          do
            Result := Show_Dialog(Dialog);
          end Show_Dialog;
        else

          -- If nothing else appeals: there is at least one window in
          -- existence, so try to pump any pending Windows messages

          while PeekMessage(M'Unchecked_Access, System.Null_Address,
                            0, 0, PM_REMOVE) /= 0 loop
            -- A message is pending, so find the top-level parent of the
            -- window it's aimed at

            H := M.hwnd;
            while GetParent(H) /= System.Null_Address loop
              H := GetParent(H);
            end loop;

            -- Now dispatch it in the classic Windows fashion, using the
            -- top-level window handle to ensure that dialog messages
            -- (TAB and other navigation keys) are translated before
            -- any other processing

            if IsDialogMessage (H, M'Unchecked_Access) = 0 then
              Bool_Dummy := TranslateMessage(M'Unchecked_Access);
              Long_Dummy := DispatchMessage(M'Unchecked_Access);
            end if;

          end loop;
          delay 0.001;        -- to avoid hogging the CPU
        end select;
      end loop;
    end loop;

  exception
    when E : others =>        -- task failure
      Window_Info.Record_Error (E);
  end Message_Loop;

  ----------------------------------------------------------------------------
  --
  --                 C A L L B A C K   F U N C T I O N S
  --
  --  These functions are called from the Windows message handler callbacks.
  --
  --  Enable (Window, Active): enable or disable the specified window
  --  Resize (Window, unused): recalculate the size of the specified window
  --
  ----------------------------------------------------------------------------

  function Enable (Window : Win32_HWND;
                   Active : Win32_LPARAM) return Win32_BOOL;
  pragma Convention(StdCall, Enable);

  function Resize (Window : Win32_HWND;
                   unused : Win32_LPARAM) return Win32_BOOL;
  pragma Convention(StdCall, Resize);

  ----------------------------------------------------------------------------
  --
  --  Enable Window to the opposite of its current state
  --
  function Enable (Window : Win32_HWND;
                   Active : Win32_LPARAM) return Win32_BOOL is
  begin
    if Window /= Window_Info.Active_Window then
      Bool_Dummy := EnableWindow (Window, Boolean'Pos(Active=0));
    end if;
    return 1;                               -- continue with next window
  end Enable;

  ----------------------------------------------------------------------------
  --
  --  Resize Window if necessary by reference to the size of its parent.
  --
  function Resize (Window : Win32_HWND; unused : Win32_LPARAM) return Win32_BOOL is
    P : Window_Ptr;
    T : Integer;
    L : Integer;
    W : Integer;
    H : Integer;
    B : Boolean;
    X : Win32_LONG := GetWindowLong(Window,GWL_USERDATA);
  begin

    -- Not all windows will have been created by this library, but all that
    -- haven't will have their user data (now in X) set to zero.

    if X /= 0 then

      -- X is really a Window_Ptr if we reach this point

      P := To_Window_Ptr(X);

      -- Copy the original (relative) coordinates
      
      T := P.Top;
      L := P.Left;
      W := P.Width;
      H := P.Height;

      -- Convert them to absolute (parent-based) coordinates

      Get_Actual_Bounds (GetParent(Window), T, L, W, H, B);

      -- B will have been set true if any of T/L/W/H are relative coordinates
      -- and T/L/W/H will have been set to absolute (parent-based) values, so
      -- resize the window if necessary

      if B then
        Bool_Dummy := SetWindowPos(Window, System.Null_Address,
                                           Win32_INT(L), Win32_INT(T),
                                           Win32_INT(W), Win32_INT(H),
                                           SWP_NOZORDER);
      end if;
    end if;

    return 1;                               -- continue with next window
  end Resize;

  ----------------------------------------------------------------------------
  --
  --                       F R A M E _ P R O C
  --
  --  The Windows message handler callback for Frame_Type windows.
  --
  ----------------------------------------------------------------------------

  function Frame_Proc (hwnd   : Win32_HWND;
                       msg    : Win32_UINT;
                       wParam : Win32_WPARAM;
                       lParam : Win32_LPARAM) return Win32_LONG is
    L : Win32_LONG := GetWindowLong(hwnd,GWL_USERDATA);
    P : Main_Window_Ptr := Main_Window_Ptr(To_Window_Ptr(L));
    H : Win32_HWND;
    N : Natural;
  begin
    case msg is

      -- Frame creation: a pointer to the Window_Internals is passed
      -- in the CREATESTRUCT pointed to by lParam, so save this for
      -- later use in the user data area of the Windows data structure
      -- (retrieved into P, above, which will be zero until initialised).

      when WM_CREATE =>
        L := To_LONG(To_CREATESTRUCT(lParam).lpCreateParams);
        Long_Dummy := SetWindowLong (hwnd, GWL_USERDATA, L);

      -- Frame activation or deactivation: save or restore the focused
      -- control for the next activation

      when WM_ACTIVATE =>
        if (wParam and 16#FFFF#) /= 0 then          -- activation
          H := SetFocus (P.Focus);
        elsif GetFocus /= System.Null_Address then  -- deactivation
          P.Focus := GetFocus;
        end if;
        return 0;                                   -- don't do anything else

      -- Frame closing: issue the command for this frame

      when WM_CLOSE =>
        Window_Info.Set_Command (P.Action + WM_USER);

      -- Frame being destroyed: decrement the frame count and clear the
      -- window handle

      when WM_DESTROY =>
        Frames := Frames - 1;

      -- Frame resized: resize all the child windows which have relative
      -- sizes or positions

      when WM_SIZE =>
        Bool_Dummy := EnumChildWindows (hwnd, Resize'Access, 0);

      -- An action has occurred (command code in low 16 bits of wParam);
      -- issue a command code if it's in the appropriate range, or ignore
      -- it if not

      when WM_COMMAND =>
        N := Natural(wParam and 16#FFFF#);
        if N in WM_USER .. 16#7FFF# then
          Window_Info.Set_Command (N);
          return 0;                                 -- don't do anything else
        end if;

      -- Ignore all other messages

      when others =>
        null;

    end case;

    -- Perform the default action for any messages that don't return before
    -- this point

    return DefWindowProc(hwnd, msg, wParam, lParam);

  end Frame_Proc;

  ----------------------------------------------------------------------------
  --
  --                      D I A L O G _ P R O C
  --
  --  The Windows message handler callback for Dialog_Type windows.
  --
  ----------------------------------------------------------------------------

  function Dialog_Proc (hwnd   : Win32_HWND;
                        msg    : Win32_UINT;
                        wParam : Win32_WPARAM;
                        lParam : Win32_LPARAM) return Win32_LONG is
    P : Window_Ptr := To_Window_Ptr(GetWindowLong(hwnd,GWL_USERDATA));
    H : Win32_HWND;
  begin
    case msg is

      -- Dialog deactivation: don't do anything more so that the current
      -- focused control isn't saved, and so the first control will get
      -- the focus the next time the dialog is activated

      when WM_ACTIVATE =>
        if (wParam and 16#FFFF#) = 0 then
          return 0;                                 -- don't do anything else
        end if;

      -- Dialog being shown/hidden: disable/enable all the other windows
      -- for this thread (i.e. this application), and set a new active
      -- window if the dialog is being hidden

      when WM_SHOWWINDOW =>
        Bool_Dummy := EnumThreadWindows (GetCurrentThreadID,
                                         Enable'Access,
                                         Win32_LONG(wParam));
        if wParam = 0 then
          H := SetActiveWindow(GetWindow(P.Handle,GW_HWNDNEXT));
        end if;

      -- Dialog closing: don't close, just hide

      when WM_CLOSE =>
        Bool_Dummy := ShowWindow (hwnd, SW_HIDE);
        Window_Info.Set_Command (P.Action + WM_USER);
        return 0;                                   -- don't do anything else

      -- Ignore all other messages

      when others =>
        null;

    end case;

    -- If nothing else has happened yet, treat this the same way as a
    -- Frame_Type top-level window

    return Frame_Proc(hwnd, msg, wParam, lParam);

  end Dialog_Proc;

  ----------------------------------------------------------------------------
  --
  --                      C A N V A S _ P R O C
  --
  --  The Windows message handler callback for Canvas_Type windows.
  --
  ----------------------------------------------------------------------------

  function Canvas_Proc (hwnd   : Win32_HWND;
                        msg    : Win32_UINT;
                        wParam : Win32_WPARAM;
                        lParam : Win32_LPARAM) return Win32_LONG is
    C : Canvas_Ptr := Canvas_Ptr(To_Window_Ptr(GetWindowLong(hwnd,GWL_USERDATA)));
    H : Win32_HWND;
    M : Win32_POINTS;
  begin
    case msg is
      -- Canvas background being erased: get the client area rectangle and
      -- fill it with the chosen background colour (from the canvas monitor)

      when WM_ERASEBKGND =>
        declare
          R : aliased Win32_RECT;
          I : Win32_INT;
        begin
          Bool_Dummy := GetClientRect(hwnd,R'Unchecked_Access);
          I := FillRect (To_HDC(wParam), R'Unchecked_Access,
                         C.Monitor.Background);
        end;
        return 1;     -- result of this message must be 1, not 0

      -- Canvas being painted: ask the monitor to draw all the objects in the
      -- drawing list

      when WM_PAINT =>
        C.Monitor.Draw (hwnd, C.Font);

      -- Mouse button down (ignored if this canvas does not generate a
      -- command): capture the mouse and ask the canvas monitor to record
      -- the mouse position and button state, then issue the command

      when WM_LBUTTONDOWN =>
        if C.Action >= 0 then
          H := SetCapture(hwnd);
          M := MakePoint (lParam);
          C.Monitor.Set_Start (Integer(M.X), Integer(M.Y));
          C.Monitor.Set_Button (True);
          Window_Info.Set_Command (C.Action + WM_USER);
          return 0;                                 -- don't do anything else
        end if;

      -- Mouse button up (ignored if this canvas does not generate a
      -- a command): release the mouse and ask the canvas monitor to
      -- record the current position and button state

      when WM_LBUTTONUP =>
        if C.Action >= 0 then
          Bool_Dummy := ReleaseCapture;
          M := MakePoint (lParam);
          C.Monitor.Set_End (Integer(M.X), Integer(M.Y));
          C.Monitor.Set_Button (False);
          return 0;                                 -- don't do anything else
        end if;

      -- Mouse has moved (ignored if this canvas does not generate
      -- a command or the mouse button isn't down): ask the canvas
      -- monitor to record the current mouse position

      when WM_MOUSEMOVE =>
        if C.Action >= 0 and C.Monitor.Mouse_Down then
          M := MakePoint (lParam);
          C.Monitor.Set_End (Integer(M.X), Integer(M.Y));
          return 0;                                 -- don't do anything else
        end if;

      -- Key pressed (ignored if this canvas does not generate a keypress
      -- command): capture the mouse and ask the canvas monitor to record
      -- the mouse position and button state, then issue the command

      when WM_CHAR =>
        if C.Keypress >= 0 then
          C.Monitor.Set_Key (Character'Val(wParam and 16#FF#));
          Window_Info.Set_Command (C.Keypress + WM_USER);
          return 0;                                 -- don't do anything else
        end if;

      -- Because messages are processed by IsDialogMessage, WM_CHAR messages
      -- won't be seen unless they are asked for in response to WM_GETDLCODE
      
      when WM_GETDLGCODE =>
        return DLGC_WANTMESSAGE;   -- request WM_CHAR for all keyboard input
        
      -- Ignore all other messages

      when others =>
        null;

    end case;
    
    -- Perform the default action for any messages that don't return before
    -- this point

    return DefWindowProc(hwnd, msg, wParam, lParam);

  end Canvas_Proc;

  ----------------------------------------------------------------------------
  --
  --                       P A N E L _ P R O C
  --
  --  The Windows message handler callback for Panel_Type windows. Command
  --  messages are sent to the parent window, all others are processed in
  --  the normal way.
  --
  ----------------------------------------------------------------------------

  function Panel_Proc (hwnd   : Win32_HWND;
                       msg    : Win32_UINT;
                       wParam : Win32_WPARAM;
                       lParam : Win32_LPARAM) return Win32_LONG is
    W : Window_Ptr := To_Window_Ptr(GetWindowLong(hwnd,GWL_USERDATA));
  begin
    case msg is
      when WM_COMMAND =>
        Long_Dummy := SendMessage (GetParent(hwnd), msg, wParam, lParam);
      when others =>
        null;
    end case;
    return CallWindowProc (W.WndProc, hwnd, msg, wParam, lParam);
  end Panel_Proc;

  ----------------------------------------------------------------------------
  --
  --                        M E M O _ P R O C
  --
  --  The Windows message handler callback for Memo_Type windows. Tab keys
  --  cause a tab to be inserted, all other messages are processed in the
  --  normal way.
  --
  ----------------------------------------------------------------------------

  function Memo_Proc (hwnd   : Win32_HWND;
                      msg    : Win32_UINT;
                      wParam : Win32_WPARAM;
                      lParam : Win32_LPARAM) return Win32_LONG is
    W : Window_Ptr := To_Window_Ptr(GetWindowLong(hwnd,GWL_USERDATA));
    S : aliased Win32_String := To_Array (ASCII.HT & ASCII.NUL);
  begin
    case msg is
      when WM_KEYDOWN =>
        if wParam = Character'Pos(ASCII.HT) then
          Long_Dummy := SendMessage (hwnd, EM_REPLACESEL, 1, To_LPARAM(S));
        end if;
      when others =>
        null;
    end case;
    return CallWindowProc (W.WndProc, hwnd, msg, wParam, lParam);
  end Memo_Proc;

end JEWL.Message_Handling;
------------------------------------------------------------------------------
--                                                                          --
--                J E W L . M E S S A G E _ H A N D L I N G                 --
--                                                                          --
--   This is a private package which defines the message-handling task      --
--   required by JEWL.Windows, the protected record used to communicate     --
--   with it, and related operations.                                       --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-message_handling.ads 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-message_handling.ads $
-- Revision 1.7  2007/01/08 17:00:00  JE
-- * Fixed linker options in JEWL.Win32_Interface to accommodate changes to GNAT
--   GPL 2006 compiler (thanks to John McCormick for this)
-- * Added delay in message loop to avoid the appearance of hogging 100% of CPU
--   time
--
-- Revision 1.6  2001/11/02 16:00:00  JE
-- * Fixed canvas bug when saving an empty canvas
-- * Restore with no prior save now acts as erase
-- * Removed redundant variable declaration in Image function
--
-- Revision 1.5  2001/08/22 15:00:00  JE
-- * Minor bugfix to Get_Text for combo boxes
-- * Minor changes to documentation (including new example involving dialogs)
--
-- Revision 1.4  2001/01/25 09:00:00  je
-- Changes visible to the user:
--
-- * Added support for drawing bitmaps on canvases (Draw_Image operations
--   and new type Image_Type)
-- * Added Play_Sound
-- * Added several new operations on all windows: Get_Origin, Get_Width,
--   Get_Height, Set_Origin, Set_Size and Focus
-- * Added several functions giving screen and window dimensions: Screen_Width,
--   Screen_Height, Frame_Width, Frame_Height, Dialog_Width, Dialog_Height and
--   Menu_Height
-- * Canvases can now handle keyboard events: new constructor and Key_Code added
-- * Added procedure Play_Sound
-- * Operations "+" and "-" added for Point_Type
-- * Pens can now be zero pixels wide
-- * The absolute origin of a frame can now have be specified when the frame
--   is created
-- * Added new File_Dialog operations Add_Filter and Set_Directory
-- * Added Get_Line renames to JEWL.IO for compatibility with Ada.Text_IO
-- * Added all the Get(File,Item) operations mentioned in documentation but
--   unaccountably missing :-(
-- * Documentation updated to reflect the above changes
-- * HTML versions of public package specifications added with links from
--   main documentation pages
--
-- Other internal changes:
--
-- * Canvas fonts, pens etc. now use JEWL.Reference_Counted_Type rather than
--   reinventing this particular wheel, as do images
-- * Various minor code formatting changes: some code reordered for clarity,
--   some comments added or amended,
-- * Changes introduced in 1.2 to support GNAT 3.10 have been reversed, since
--   GNAT 3.10 still couldn't compile this code correctly... ;-(
--
-- Outstanding issues:
--
-- * Optimisation breaks the code (workaround: don't optimise)
--
-- Revision 1.3  2000/07/07 12:00:00  je
-- * JEWL.Simple_Windows added; JEWL.IO modified to use JEWL.Simple_Windows.
-- * JEWL.IO bug fix: Put_Line to file wrote newline to standard output
--   instead of to the file (thanks to Jeff Carter for pointing this out).
-- * Panels fixed so that mouse clicks are passed on correctly to subwindows.
-- * Memos fixed so that tabs are handled properly.
-- * Password feature added to editboxes.
-- * Minor typos fixed in comments within the package sources.
-- * Documentation corrected and updated following comments from Moti Ben-Ari
--   and Don Overheu.
--
-- Revision 1.2  2000/04/18 20:00:00  je
-- * Minor code changes to enable compilation by GNAT 3.10
-- * Minor documentation errors corrected
-- * Some redundant "with" clauses removed
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

with JEWL.Win32_Interface;       use JEWL.Win32_Interface;
with JEWL.Window_Implementation; use JEWL.Window_Implementation;

with Ada.Exceptions;
with System;

private package JEWL.Message_Handling is

  ----------------------------------------------------------------------------
  --
  --                       M E S S A G E _ L O O P
  --
  --  This task is responsible for pumping the Windows message loop.
  --  Windows requires that all windows are created and destroyed by
  --  the same task that will handle their messages, so task entries
  --  are used to ask the message task to create and destroy windows.
  --  The entries are:
  --
  --  Create_Window  : create a top-level window, given the name of the
  --                   window class, the window title, extended and normal
  --                   window styles, and whether the window should be
  --                   made visible.
  --  Create_Child   : create a child window, given the parent window's
  --                   handle, the name of the window class, the window
  --                   title, style, and coordinates.
  --  Show_Dialog    : show a common dialog and return its result.
  --  Destroy_Window : destroy a window (specified by its handle).
  --
  ----------------------------------------------------------------------------

  task Message_Loop is
    entry Create_Child   (Window : in Window_Ptr;
                          Parent : in Container_Ptr;
                          Class  : in Win32_String;
                          Title  : in Win32_String;
                          XStyle : in Win32_DWORD;
                          Style  : in Win32_DWORD;
                          Top    : in Integer;
                          Left   : in Integer;
                          Width  : in Integer;
                          Height : in Integer);
    entry Create_Window  (Window : in Main_Window_Ptr;
                          Class  : in Win32_String;
                          Title  : in Win32_String;
                          XStyle : in Win32_DWORD;
                          Style  : in Win32_DWORD;
                          Show   : in Boolean);
    entry Show_Dialog    (Dialog : in Common_Dialog_Ptr;
                          Result : out Boolean);
    entry Set_Focus      (Window : in Win32_HWND);
    entry Destroy_Window (Handle : in Win32_HWND);
  end Message_Loop;

  ----------------------------------------------------------------------------
  --
  --                        W I N D O W _ I N F O
  --
  --  This protected record is used for global communication between the
  --  message loop task and the main task. The private data this contains
  --  is as follows:
  --
  --  Command       : the current command code (0 if no command available)
  --  Dialog        : the current active dialog (null if no dialog active)
  --  Task_Failed   : True if the message loop task has failed
  --  Failure_Info  : the exception that caused the message loop to fail
  --
  --  The operations provide are as follows:
  --
  --  Get_Command   : wait for a command and then return its code
  --  Test_Command  : test if there is a command pending
  --  Set_Command   : set the current command
  --  Get_Dialog    : get the handle of the current dialog (null if none)
  --  Active_Window : set the handle of the active dialog window and get
  --                  the old value of the handle
  --  Record_Error  : record an exception that caused the message loop
  --                  to fail
  --
  ----------------------------------------------------------------------------

  protected Window_Info is
    entry     Get_Command   (Cmd : out Natural);
    function  Test_Command  return Boolean;
    procedure Set_Command   (Cmd : in  Natural);
    procedure Get_Dialog    (Dlg : in out Win32_HWND);
    function  Active_Window return Win32_HWND;
    procedure Record_Error  (Err : in Ada.Exceptions.Exception_Occurrence);
  private
    Command      : Natural := 0;
    Dialog       : Win32_HWND := System.Null_Address;
    Task_Failed  : Boolean := False;
    Failure_Info : Ada.Exceptions.Exception_Occurrence;
  end Window_Info;

  ----------------------------------------------------------------------------
  --
  --                   M E S S A G E   H A N D L E R S
  --
  --  These functions are associated with the window classes for frames,
  --  dialogs and canvases when the window classes are registered (in the
  --  initialisation section at the end of the body of JEWL.Windows).
  --  Windows will call the appropriate function when a message is sent
  --  to a window belonging to the corresponding class, specifying the
  --  window handle, message code and any additional parameters.
  --
  ----------------------------------------------------------------------------

  function Frame_Proc  (hwnd   : Win32_HWND;
                        msg    : Win32_UINT;
                        wParam : Win32_WPARAM;
                        lParam : Win32_LPARAM) return Win32_LONG;
                      pragma Convention(StdCall, Frame_Proc);

  function Dialog_Proc (hwnd   : Win32_HWND;
                        msg    : Win32_UINT;
                        wParam : Win32_WPARAM;
                        lParam : Win32_LPARAM) return Win32_LONG;
                      pragma Convention(StdCall, Dialog_Proc);

  function Canvas_Proc (hwnd   : Win32_HWND;
                        msg    : Win32_UINT;
                        wParam : Win32_WPARAM;
                        lParam : Win32_LPARAM) return Win32_LONG;
                      pragma Convention(StdCall, Canvas_Proc);

  function Panel_Proc  (hwnd   : Win32_HWND;
                        msg    : Win32_UINT;
                        wParam : Win32_WPARAM;
                        lParam : Win32_LPARAM) return Win32_LONG;
                      pragma Convention(StdCall, Panel_Proc);

  function Memo_Proc   (hwnd   : Win32_HWND;
                        msg    : Win32_UINT;
                        wParam : Win32_WPARAM;
                        lParam : Win32_LPARAM) return Win32_LONG;
                      pragma Convention(StdCall, Memo_Proc);

  ----------------------------------------------------------------------------
  --
  --                 U T I L I T Y   P R O C E D U R E S
  --
  --  Get_Actual_Bounds : test if a set of window dimensions is relative to
  --                      the dimensions of the parent. If they are, set the
  --                      Resize parameter True and set the dimensions to
  --                      the corresponding absolute (parent-based) values.
  --
  ----------------------------------------------------------------------------

  procedure Get_Actual_Bounds (Parent  : in Win32_HWND;
                               Top     : in out Integer;
                               Left    : in out Integer;
                               Width   : in out Integer;
                               Height  : in out Integer;
                               Resize  : out Boolean);

end JEWL.Message_Handling;
------------------------------------------------------------------------------
--                                                                          --
--                  J E W L . S I M P L E _ W I N D O W S                   --
--                                                                          --
--   A predefined instantiation of JEWL.Windows for type Character.         --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-simple_windows.ads 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-simple_windows.ads $
-- Revision 1.7  2007/01/08 17:00:00  JE
-- * Fixed linker options in JEWL.Win32_Interface to accommodate changes to GNAT
--   GPL 2006 compiler (thanks to John McCormick for this)
-- * Added delay in message loop to avoid the appearance of hogging 100% of CPU
--   time
--
-- Revision 1.6  2001/11/02 16:00:00  JE
-- * Fixed canvas bug when saving an empty canvas
-- * Restore with no prior save now acts as erase
-- * Removed redundant variable declaration in Image function
--
-- Revision 1.5  2001/08/22 15:00:00  JE
-- * Minor bugfix to Get_Text for combo boxes
-- * Minor changes to documentation (including new example involving dialogs)
--
-- Revision 1.4  2001/01/25 09:00:00  je
-- Changes visible to the user:
--
-- * Added support for drawing bitmaps on canvases (Draw_Image operations
--   and new type Image_Type)
-- * Added Play_Sound
-- * Added several new operations on all windows: Get_Origin, Get_Width,
--   Get_Height, Set_Origin, Set_Size and Focus
-- * Added several functions giving screen and window dimensions: Screen_Width,
--   Screen_Height, Frame_Width, Frame_Height, Dialog_Width, Dialog_Height and
--   Menu_Height
-- * Canvases can now handle keyboard events: new constructor and Key_Code added
-- * Added procedure Play_Sound
-- * Operations "+" and "-" added for Point_Type
-- * Pens can now be zero pixels wide
-- * The absolute origin of a frame can now have be specified when the frame
--   is created
-- * Added new File_Dialog operations Add_Filter and Set_Directory
-- * Added Get_Line renames to JEWL.IO for compatibility with Ada.Text_IO
-- * Added all the Get(File,Item) operations mentioned in documentation but
--   unaccountably missing :-(
-- * Documentation updated to reflect the above changes
-- * HTML versions of public package specifications added with links from
--   main documentation pages
--
-- Other internal changes:
--
-- * Canvas fonts, pens etc. now use JEWL.Reference_Counted_Type rather than
--   reinventing this particular wheel, as do images
-- * Various minor code formatting changes: some code reordered for clarity,
--   some comments added or amended,
-- * Changes introduced in 1.2 to support GNAT 3.10 have been reversed, since
--   GNAT 3.10 still couldn't compile this code correctly... ;-(
--
-- Outstanding issues:
--
-- * Optimisation breaks the code (workaround: don't optimise)
--
-- Revision 1.3  2000/07/07 12:00:00  je
-- * JEWL.Simple_Windows added; JEWL.IO modified to use JEWL.Simple_Windows.
-- * JEWL.IO bug fix: Put_Line to file wrote newline to standard output
--   instead of to the file (thanks to Jeff Carter for pointing this out).
-- * Panels fixed so that mouse clicks are passed on correctly to subwindows.
-- * Memos fixed so that tabs are handled properly.
-- * Password feature added to editboxes.
-- * Minor typos fixed in comments within the package sources.
-- * Documentation corrected and updated following comments from Moti Ben-Ari
--   and Don Overheu.
--
------------------------------------------------------------------------------

with JEWL.Windows;
package JEWL.Simple_Windows is new JEWL.Windows (Command_Type => Character);
------------------------------------------------------------------------------
--                                                                          --
--                 J E W L . W I N 3 2 _ I N T E R F A C E                  --
--                                                                          --
--   This is the body of a private package containing implementation        --
--   details for JEWL.Windows. It contains type conversions where a         --
--   bit-for-bit conversion is inadequate. It also contains some            --
--   functions which provide a more tasteful Ada wrapping for a few         --
--   common sequences of Win32 magic spells.                                --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-win32_interface.adb 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-win32_interface.adb $
-- Revision 1.7  2007/01/08 17:00:00  JE
-- * Fixed linker options in JEWL.Win32_Interface to accommodate changes to GNAT
--   GPL 2006 compiler (thanks to John McCormick for this)
-- * Added delay in message loop to avoid the appearance of hogging 100% of CPU
--   time
--
-- Revision 1.6  2001/11/02 16:00:00  JE
-- * Fixed canvas bug when saving an empty canvas
-- * Restore with no prior save now acts as erase
-- * Removed redundant variable declaration in Image function
--
-- Revision 1.5  2001/08/22 15:00:00  JE
-- * Minor bugfix to Get_Text for combo boxes
-- * Minor changes to documentation (including new example involving dialogs)
--
-- Revision 1.4  2001/01/25 09:00:00  je
-- Changes visible to the user:
--
-- * Added support for drawing bitmaps on canvases (Draw_Image operations
--   and new type Image_Type)
-- * Added Play_Sound
-- * Added several new operations on all windows: Get_Origin, Get_Width,
--   Get_Height, Set_Origin, Set_Size and Focus
-- * Added several functions giving screen and window dimensions: Screen_Width,
--   Screen_Height, Frame_Width, Frame_Height, Dialog_Width, Dialog_Height and
--   Menu_Height
-- * Canvases can now handle keyboard events: new constructor and Key_Code added
-- * Added procedure Play_Sound
-- * Operations "+" and "-" added for Point_Type
-- * Pens can now be zero pixels wide
-- * The absolute origin of a frame can now have be specified when the frame
--   is created
-- * Added new File_Dialog operations Add_Filter and Set_Directory
-- * Added Get_Line renames to JEWL.IO for compatibility with Ada.Text_IO
-- * Added all the Get(File,Item) operations mentioned in documentation but
--   unaccountably missing :-(
-- * Documentation updated to reflect the above changes
-- * HTML versions of public package specifications added with links from
--   main documentation pages
--
-- Other internal changes:
--
-- * Canvas fonts, pens etc. now use JEWL.Reference_Counted_Type rather than
--   reinventing this particular wheel, as do images
-- * Various minor code formatting changes: some code reordered for clarity,
--   some comments added or amended,
-- * Changes introduced in 1.2 to support GNAT 3.10 have been reversed, since
--   GNAT 3.10 still couldn't compile this code correctly... ;-(
--
-- Outstanding issues:
--
-- * Optimisation breaks the code (workaround: don't optimise)
--
-- Revision 1.3  2000/07/07 12:00:00  je
-- * JEWL.Simple_Windows added; JEWL.IO modified to use JEWL.Simple_Windows.
-- * JEWL.IO bug fix: Put_Line to file wrote newline to standard output
--   instead of to the file (thanks to Jeff Carter for pointing this out).
-- * Panels fixed so that mouse clicks are passed on correctly to subwindows.
-- * Memos fixed so that tabs are handled properly.
-- * Password feature added to editboxes.
-- * Minor typos fixed in comments within the package sources.
-- * Documentation corrected and updated following comments from Moti Ben-Ari
--   and Don Overheu.
--
-- Revision 1.2  2000/04/18 20:00:00  je
-- * Minor code changes to enable compilation by GNAT 3.10
-- * Minor documentation errors corrected
-- * Some redundant "with" clauses removed
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

with Interfaces.C;

package body JEWL.Win32_Interface is

  pragma Linker_Options ("-luser32");
  pragma Linker_Options ("-lgdi32");
  pragma Linker_Options ("-lcomdlg32");
  pragma Linker_Options ("-lwinmm");

  use type Win32_DWORD, Win32_INT, Win32_UINT, Win32_LONG, Win32_BYTE;
  use type System.Address;

  ----------------------------------------------------------------------------
  --
  --                   T Y P E   C O N V E R S I O N S
  --
  ----------------------------------------------------------------------------

  function To_LPSTR (S : Win32_String) return Win32_LPSTR is
    function UC is new Ada.Unchecked_Conversion
                            (System.Address, Win32_LPSTR);
  begin
    return UC(S(S'First)'Address);
  end To_LPSTR;

  function To_LPCSTR (S : Win32_String) return Win32_LPCSTR is
    function UC is new Ada.Unchecked_Conversion
                            (System.Address, Win32_LPCSTR);
  begin
    return UC(S(S'First)'Address);
  end To_LPCSTR;

  function To_LPARAM (S : Win32_String) return Win32_LPARAM is
    function UC is new Ada.Unchecked_Conversion
                            (System.Address, Win32_LPARAM);
  begin
    return UC(S(S'First)'Address);
  end To_LPARAM;

  function To_String (S : Win32_String) return String is
  begin
    return Interfaces.C.To_Ada(S);
  end To_String;

  function To_Array (S : String) return Win32_String is
  begin
    return Interfaces.C.To_C(S);
  end To_Array;

  function RGB (Colour : Colour_Type) return Win32_COLORREF is
    use type Win32_BYTE;
  begin
    return Win32_COLORREF(Colour.Red + Colour.Green*2**8 + Colour.Blue*2**16);
  end RGB;
  
  function MakePoint (Value: Win32_LPARAM) return Win32_POINTS is
    use type Interfaces.Unsigned_32;
    function UC is new Ada.Unchecked_Conversion
                           (Win32_LPARAM,Interfaces.Unsigned_32);
    function UC is new Ada.Unchecked_Conversion
                           (Win32_WORD,Win32_SHORT);
    P : Win32_POINTS;
    V : Interfaces.Unsigned_32 := UC(Value);
  begin
    P.X := UC(Win32_WORD(V and 16#FFFF#));
    P.Y := UC(Win32_WORD(Interfaces.Shift_Right(V,16) and 16#FFFF#));
    return P;
  end MakePoint;
  
  ----------------------------------------------------------------------------
  --
  --                  U T I L I T Y   F U N C T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Message_Box: an Ada wrapper for the Win32 MessageBox function.
  --
  function Message_Box (Message : String;
                        Title   : String;
                        Flags   : Win32_UINT) return Integer is
    M : Win32_String := To_Array(Message);
    T : Win32_String := To_Array(Title);
  begin
    return Integer(MessageBox(System.Null_Address,
                              To_LPCSTR(M), To_LPCSTR(T),
                              Flags or MB_TASKMODAL or MB_SETFOREGROUND));
  end Message_Box;

  ----------------------------------------------------------------------------
  --
  --  Create_Font: an Ada wrapper for the Win32 CreateFont function.
  --
  function Create_Font (Font : Font_Type) return Win32_HFONT is
    F : Win32_HFONT;
    L : aliased Win32_LOGFONT := Set_Font(Font);
  begin
    F := CreateFontIndirect (L'Unchecked_Access);
    return F;
  end Create_Font;

  ----------------------------------------------------------------------------
  --
  --  Set_Font: convert a Font_Type object to a Win32 font.
  --
  function Set_Font (Font : Font_Type) return Win32_LOGFONT is
    F : Win32_LOGFONT;
    H : Win32_HDC;
    I : Win32_INT;
  begin
    H := GetDC(System.Null_Address);
    F.lfHeight := -Win32_LONG(GetDeviceCaps(H,LOGPIXELSY) *
                              Win32_INT(Font.Size) / 72);
    I := ReleaseDC(System.Null_Address,H);
    if Font.Bold then
      F.lfWeight := 700;
    else
      F.lfWeight := 400;
    end if;
    F.lfItalic := Boolean'Pos(Font.Italic);
    if Font.Name'Length < F.lfFaceName'Length then
      F.lfFaceName(0..Font.Name'Length) := To_Array(Font.Name);
    else
      F.lfFaceName := To_Array(Font.Name(1..F.lfFaceName'Length-1));
    end if;
    return F;
  end Set_Font;

  ----------------------------------------------------------------------------
  --
  --  Get_Font: convert a Win32 font to a Font_Type object.
  --
  function Get_Font (Font : Win32_LOGFONT) return Font_Type is
    H : Win32_HDC;
    I : Win32_INT;
    S : Float;
  begin
    H := GetDC(System.Null_Address);
    S := 72.0 / Float(GetDeviceCaps(H,LOGPIXELSY));
    I := ReleaseDC (System.Null_Address, H);
    return JEWL.Font(To_String(Font.lfFaceName),
                     abs Integer(Float(Font.lfHeight)*S),
                     Font.lfWeight > 500, Font.lfItalic /= 0);
  end Get_Font;

end JEWL.Win32_Interface;
------------------------------------------------------------------------------
--                                                                          --
--                 J E W L . W I N 3 2 _ I N T E R F A C E                  --
--                                                                          --
--   This is a private package containing definitions relating to the       --
--   use of the underlying Win32 API targetted by this implementation       --
--   of the JEWL.Windows package.                                           --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-win32_interface.ads 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-win32_interface.ads $
-- Revision 1.7  2007/01/08 17:00:00  JE
-- * Fixed linker options in JEWL.Win32_Interface to accommodate changes to GNAT
--   GPL 2006 compiler (thanks to John McCormick for this)
-- * Added delay in message loop to avoid the appearance of hogging 100% of CPU
--   time
--
-- Revision 1.6  2001/11/02 16:00:00  JE
-- * Fixed canvas bug when saving an empty canvas
-- * Restore with no prior save now acts as erase
-- * Removed redundant variable declaration in Image function
--
-- Revision 1.5  2001/08/22 15:00:00  JE
-- * Minor bugfix to Get_Text for combo boxes
-- * Minor changes to documentation (including new example involving dialogs)
--
-- Revision 1.4  2001/01/25 09:00:00  je
-- Changes visible to the user:
--
-- * Added support for drawing bitmaps on canvases (Draw_Image operations
--   and new type Image_Type)
-- * Added Play_Sound
-- * Added several new operations on all windows: Get_Origin, Get_Width,
--   Get_Height, Set_Origin, Set_Size and Focus
-- * Added several functions giving screen and window dimensions: Screen_Width,
--   Screen_Height, Frame_Width, Frame_Height, Dialog_Width, Dialog_Height and
--   Menu_Height
-- * Canvases can now handle keyboard events: new constructor and Key_Code added
-- * Added procedure Play_Sound
-- * Operations "+" and "-" added for Point_Type
-- * Pens can now be zero pixels wide
-- * The absolute origin of a frame can now have be specified when the frame
--   is created
-- * Added new File_Dialog operations Add_Filter and Set_Directory
-- * Added Get_Line renames to JEWL.IO for compatibility with Ada.Text_IO
-- * Added all the Get(File,Item) operations mentioned in documentation but
--   unaccountably missing :-(
-- * Documentation updated to reflect the above changes
-- * HTML versions of public package specifications added with links from
--   main documentation pages
--
-- Other internal changes:
--
-- * Canvas fonts, pens etc. now use JEWL.Reference_Counted_Type rather than
--   reinventing this particular wheel, as do images
-- * Various minor code formatting changes: some code reordered for clarity,
--   some comments added or amended,
-- * Changes introduced in 1.2 to support GNAT 3.10 have been reversed, since
--   GNAT 3.10 still couldn't compile this code correctly... ;-(
--
-- Outstanding issues:
--
-- * Optimisation breaks the code (workaround: don't optimise)
--
-- Revision 1.3  2000/07/07 12:00:00  je
-- * JEWL.Simple_Windows added; JEWL.IO modified to use JEWL.Simple_Windows.
-- * JEWL.IO bug fix: Put_Line to file wrote newline to standard output
--   instead of to the file (thanks to Jeff Carter for pointing this out).
-- * Panels fixed so that mouse clicks are passed on correctly to subwindows.
-- * Memos fixed so that tabs are handled properly.
-- * Password feature added to editboxes.
-- * Minor typos fixed in comments within the package sources.
-- * Documentation corrected and updated following comments from Moti Ben-Ari
--   and Don Overheu.
--
-- Revision 1.2  2000/04/18 20:00:00  je
-- * Minor code changes to enable compilation by GNAT 3.10
-- * Minor documentation errors corrected
-- * Some redundant "with" clauses removed
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with System;
with Interfaces.C;

private package JEWL.Win32_Interface is

  use type Interfaces.C.Unsigned_Long;   -- reported by Pierre Breguet to be
                                         -- required by Object Ada compiler

  ----------------------------------------------------------------------------
  --  Win32 flags and constants
  ----------------------------------------------------------------------------

  BI_BITFIELDS           : constant := 3;
  BITMAP_MAGIC           : constant := 16#4D42#;        -- "BM"
  BLACK_PEN              : constant := 7;
  BM_GETCHECK            : constant := 240;
  BM_SETCHECK            : constant := 241;
  BS_AUTOCHECKBOX        : constant := 16#00000003#;
  BS_AUTORADIOBUTTON     : constant := 16#00000009#;
  BS_DEFPUSHBUTTON       : constant := 16#00000001#;
  BS_GROUPBOX            : constant := 16#00000007#;
  BS_HOLLOW              : constant := 1;
  BS_PUSHBUTTON          : constant := 16#00000000#;
  CBM_INIT               : constant := 4;
  CBS_AUTOHSCROLL        : constant := 16#00000040#;
  CBS_DROPDOWN           : constant := 16#00000002#;
  CBS_DROPDOWNLIST       : constant := 16#00000003#;
  CB_ADDSTRING           : constant := 323;
  CB_DELETESTRING        : constant := 324;
  CB_GETCOUNT            : constant := 326;
  CB_GETCURSEL           : constant := 327;
  CB_GETLBTEXT           : constant := 328;
  CB_GETLBTEXTLEN        : constant := 329;
  CB_INSERTSTRING        : constant := 330;
  CB_RESETCONTENT        : constant := 331;
  CB_SETCURSEL           : constant := 334;
  CC_RGBINIT             : constant := 16#00000001#;
  CF_FORCEFONTEXIST      : constant := 16#00010000#;
  CF_INITTOLOGFONTSTRUCT : constant := 16#00000040#;
  CF_SCREENFONTS         : constant := 16#00000001#;
  COLOR_BTNFACE          : constant := 15;
  CS_HREDRAW             : constant := 16#00000002#;
  CS_VREDRAW             : constant := 16#00000001#;
  CW_USEDEFAULT          : constant := Interfaces.C.Int'First;
  DIB_RGB_COLORS         : constant := 0;
  DLGC_WANTMESSAGE       : constant := 16#0004#;
  DT_CENTER              : constant := 16#00000001#;
  DT_LEFT                : constant := 16#00000000#;
  DT_NOCLIP              : constant := 16#00000100#;
  DT_RIGHT               : constant := 16#00000002#;
  EM_GETLINE             : constant := 196;
  EM_GETLINECOUNT        : constant := 186;
  EM_GETMODIFY           : constant := 184;
  EM_GETSEL              : constant := 176;
  EM_LINEFROMCHAR        : constant := 201;
  EM_LINEINDEX           : constant := 187;
  EM_LINELENGTH          : constant := 193;
  EM_REPLACESEL          : constant := 194;
  EM_SCROLLCARET         : constant := 183;
  EM_SETMODIFY           : constant := 185;
  EM_SETSEL              : constant := 177;
  EM_SETTABSTOPS         : constant := 203;
  ES_AUTOHSCROLL         : constant := 16#0080#;
  ES_AUTOVSCROLL         : constant := 16#0040#;
  ES_MULTILINE           : constant := 16#0004#;
  ES_NOHIDESEL           : constant := 16#0100#;
  ES_PASSWORD            : constant := 16#0020#;
  ES_WANTRETURN          : constant := 16#1000#;
  GWL_USERDATA           : constant := -21;
  GWL_WNDPROC            : constant := -4;
  GW_HWNDNEXT            : constant := 2;
  IDC_ARROW              : constant := 32512;
  IDI_APPLICATION        : constant := 32512;
  IDYES                  : constant := 6;
  LB_ADDSTRING           : constant := 384;
  LB_DELETESTRING        : constant := 386;
  LB_GETCOUNT            : constant := 395;
  LB_GETCURSEL           : constant := 392;
  LB_GETTEXT             : constant := 393;
  LB_GETTEXTLEN          : constant := 394;
  LB_INSERTSTRING        : constant := 385;
  LB_RESETCONTENT        : constant := 388;
  LB_SETCURSEL           : constant := 390;
  LOGPIXELSX             : constant := 88;
  LOGPIXELSY             : constant := 90;
  MB_ICONINFORMATION     : constant := 16#00000040#;
  MB_ICONQUESTION        : constant := 16#00000020#;
  MB_ICONSTOP            : constant := 16#00000010#;
  MB_OK                  : constant := 16#00000000#;
  MB_SETFOREGROUND       : constant := 16#00010000#;
  MB_TASKMODAL           : constant := 16#00002000#;
  MB_YESNO               : constant := 16#00000004#;
  MF_BYCOMMAND           : constant := 16#0000#;
  MF_CHECKED             : constant := 16#0008#;
  MF_DISABLED            : constant := 16#0002#;
  MF_ENABLED             : constant := 16#0000#;
  MF_GRAYED              : constant := 16#0001#;
  MF_POPUP               : constant := 16#0010#;
  MF_SEPARATOR           : constant := 16#0800#;
  MF_STRING              : constant := 16#0000#;
  MF_UNCHECKED           : constant := 16#0000#;
  NULL_BRUSH             : constant := 5;
  NULL_PEN               : constant := 8;
  OFN_CREATEPROMPT       : constant := 16#00002000#;
  OFN_FILEMUSTEXIST      : constant := 16#00001000#;
  OFN_HIDEREADONLY       : constant := 16#00000004#;
  OFN_OVERWRITEPROMPT    : constant := 16#00000002#;
  OFN_PATHMUSTEXIST      : constant := 16#00000800#;
  PM_REMOVE              : constant := 1;
  SM_CXDLGFRAME          : constant := 7;
  SM_CXEDGE              : constant := 45;
  SM_CXFRAME             : constant := 32;
  SM_CXSCREEN            : constant := 0;
  SM_CYCAPTION           : constant := 4;
  SM_CYDLGFRAME          : constant := 8;
  SM_CYEDGE              : constant := 46;
  SM_CYFRAME             : constant := 33;
  SM_CYMENU              : constant := 15;
  SM_CYSCREEN            : constant := 1;
  SND_ASYNC              : constant := 16#00000001#;
  SND_FILENAME           : constant := 16#00020000#;
  SND_NODEFAULT          : constant := 16#00000002#;
  SS_CENTER              : constant := 16#00000001#;
  SS_ETCHEDFRAME         : constant := 16#00000012#;
  SS_NOPREFIX            : constant := 16#00000080#;
  SS_RIGHT               : constant := 16#00000002#;
  SWP_NOMOVE             : constant := 16#00000002#;
  SWP_NOSIZE             : constant := 16#00000001#;
  SWP_NOZORDER           : constant := 16#00000004#;
  SW_HIDE                : constant := 0;
  SW_SHOW                : constant := 5;
  SW_SHOWNORMAL          : constant := 1;
  TRANSPARENT            : constant := 1;
  WM_ACTIVATE            : constant := 6;
  WM_CHAR                : constant := 258;
  WM_CLOSE               : constant := 16;
  WM_COMMAND             : constant := 273;
  WM_COPY                : constant := 769;
  WM_CREATE              : constant := 1;
  WM_CUT                 : constant := 768;
  WM_DESTROY             : constant := 2;
  WM_ERASEBKGND          : constant := 20;
  WM_GETDLGCODE          : constant := 135;
  WM_GETTEXT             : constant := 13;
  WM_GETTEXTLENGTH       : constant := 14;
  WM_KEYDOWN             : constant := 256;
  WM_LBUTTONDOWN         : constant := 513;
  WM_LBUTTONUP           : constant := 514;
  WM_MOUSEMOVE           : constant := 512;
  WM_PAINT               : constant := 15;
  WM_PASTE               : constant := 770;
  WM_SETFONT             : constant := 48;
  WM_SETTEXT             : constant := 12;
  WM_SHOWWINDOW          : constant := 24;
  WM_SIZE                : constant := 5;
  WM_UNDO                : constant := 772;
  WS_BORDER              : constant := 16#00800000#;
  WS_CHILD               : constant := 16#40000000#;
  WS_DLGFRAME            : constant := 16#00400000#;
  WS_EX_APPWINDOW        : constant := 16#00040000#;
  WS_EX_CLIENTEDGE       : constant := 16#00000200#;
  WS_EX_CONTROLPARENT    : constant := 16#00010000#;
  WS_GROUP               : constant := 16#00020000#;
  WS_HSCROLL             : constant := 16#00100000#;
  WS_OVERLAPPEDWINDOW    : constant := 16#00CF0000#;
  WS_SYSMENU             : constant := 16#00080000#;
  WS_TABSTOP             : constant := 16#00010000#;
  WS_VISIBLE             : constant := 16#10000000#;
  WS_VSCROLL             : constant := 16#00200000#;

  ----------------------------------------------------------------------------
  --  Win32 data types
  ----------------------------------------------------------------------------

  subtype Win32_ATOM           is Interfaces.C.Unsigned_Short;
  subtype Win32_BOOL           is Interfaces.C.Int;
  subtype Win32_BYTE           is Interfaces.C.Unsigned_Char;
  subtype Win32_CHAR           is Interfaces.C.Char;
  subtype Win32_DWORD          is Interfaces.C.Unsigned_Long;
  subtype Win32_WORD           is Interfaces.C.Unsigned_Short;
  subtype Win32_INT            is Interfaces.C.Int;
  subtype Win32_LONG           is Interfaces.C.Long;
  subtype Win32_SHORT          is Interfaces.C.Short;
  subtype Win32_UINT           is Interfaces.C.Unsigned;
  subtype Win32_WPARAM         is Interfaces.C.Unsigned;
  subtype Win32_LPARAM         is Interfaces.C.Long;
  subtype Win32_COLORREF       is Interfaces.C.Unsigned_Long;
  subtype Win32_SIZE           is Interfaces.C.Size_T;

  subtype Win32_String         is Interfaces.C.Char_Array;
  type    Win32_LPCSTR         is access constant Interfaces.C.Char;
  type    Win32_LPSTR          is access all Interfaces.C.Char;

  subtype Win32_LPVOID         is System.Address;
  subtype Win32_HANDLE         is System.Address;
  subtype Win32_HWND           is System.Address;
  subtype Win32_HBRUSH         is System.Address;
  subtype Win32_HBITMAP        is System.Address;
  subtype Win32_HDC            is System.Address;
  subtype Win32_HGDIOBJ        is System.Address;
  subtype Win32_HFONT          is System.Address;
  subtype Win32_HMENU          is System.Address;
  subtype Win32_HCURSOR        is System.Address;
  subtype Win32_HICON          is System.Address;
  subtype Win32_HPEN           is System.Address;
  subtype Win32_HINSTANCE      is System.Address;
  subtype Win32_HMODULE        is System.Address;

  type Win32_WNDPROC is
    access function (hWnd   : Win32_HWND;
                     Msg    : Win32_UINT;
                     wParam : Win32_WPARAM;
                     lParam : Win32_LPARAM) return Win32_LONG;
  pragma Convention (Stdcall, Win32_WNDPROC);

  type Win32_HOOKPROC is
    access function (hWnd   : Win32_HWND;
                     Msg    : Win32_UINT;
                     wParam : Win32_WPARAM;
                     lParam : Win32_LPARAM) return Win32_UINT;
  pragma Convention (Stdcall, Win32_HOOKPROC);

  type Win32_WNDENUMPROC is
    access function (hWnd   : Win32_HWND;
                     lParam : Win32_LPARAM) return Win32_BOOL;
  pragma Convention (Stdcall, Win32_WNDENUMPROC);

  type Win32_RECT is
    record
      Left   : Win32_LONG;
      Top    : Win32_LONG;
      Right  : Win32_LONG;
      Bottom : Win32_LONG;
    end record;

  type Win32_CREATESTRUCT is
    record
      lpCreateParams : Win32_HANDLE;
      hInstance      : Win32_HANDLE;
      hMenu          : Win32_HMENU;
      hWndParent     : Win32_HWND;
      CY             : Win32_INT;
      CX             : Win32_INT;
      Y              : Win32_INT;
      X              : Win32_INT;
      Style          : Win32_LONG;
      lpszName       : Win32_LPCSTR;
      lpszClass      : Win32_LPCSTR;
      dwExStyle      : Win32_DWORD;
    end record;

  type Win32_POINT is
    record
      X,Y : Win32_LONG;
    end record;

  type Win32_POINTS is
    record
      X,Y : Win32_SHORT;
    end record;

  type Win32_PAINTSTRUCT is
    record
      hDC        : Win32_HDC;
      fErase     : Win32_BOOL;
      rcPaint    : Win32_RECT;
      fRestore   : Win32_BOOL;
      fIncUpdate : Win32_BOOL;
      Reserved   : Win32_String (0..31);
    end record;

  type Win32_LOGBRUSH is
    record
      lbStyle : Win32_UINT;
      lbColor : Win32_COLORREF;
      lbHatch : Win32_LONG;
    end record;

  type Win32_LOGFONT is
    record
      lfHeight         : Win32_LONG;
      lfWidth          : Win32_LONG := 0;
      lfEscapement     : Win32_LONG := 0;
      lfOrientation    : Win32_LONG := 0;
      lfWeight         : Win32_LONG;
      lfItalic         : Win32_BYTE;
      lfUnderline      : Win32_BYTE := 0;
      lfStrikeOut      : Win32_BYTE := 0;
      lfCharSet        : Win32_BYTE := 0;
      lfOutPrecision   : Win32_BYTE := 0;
      lfClipPrecision  : Win32_BYTE := 0;
      lfQuality        : Win32_BYTE := 0;
      lfPitchAndFamily : Win32_BYTE := 0;
      lfFaceName       : Win32_String(0..31);
    end record;

  type Win32_MSG is
    record
      hWnd    : Win32_HWND;
      Message : Win32_UINT;
      wParam  : Win32_WPARAM;
      lParam  : Win32_LPARAM;
      Time    : Win32_DWORD;
      Point   : Win32_POINT;
    end record;

  type Win32_WNDCLASS is
    record
      Style         : Win32_UINT;
      lpfnWndProc   : Win32_WNDPROC;
      cbClsExtra    : Win32_INT;
      cbWndExtra    : Win32_INT;
      hInstance     : Win32_HINSTANCE;
      hIcon         : Win32_HICON;
      hCursor       : Win32_HCURSOR;
      hbrBackground : Win32_HBRUSH;
      lpszMenuName  : Win32_LPCSTR;
      lpszClassName : Win32_LPCSTR;
    end record;

    type Win32_BITMAPFILEHEADER is
      record
        bfType      : Win32_WORD;
        bfSize      : Win32_DWORD;
        bfReserved1 : Win32_WORD;
        bfReserved2 : Win32_WORD;
        bfOffBits   : Win32_DWORD;
      end record;

    type Win32_BITMAPINFOHEADER is
      record
        biSize          : Win32_DWORD;
        biWidth         : Win32_LONG;
        biHeight        : Win32_LONG;
        biPlanes        : Win32_WORD;
        biBitCount      : Win32_WORD;
        biCompression   : Win32_DWORD;
        biSizeImage     : Win32_DWORD;
        biXPelsPerMeter : Win32_LONG;
        biYPelsPerMeter : Win32_LONG;
        biClrUsed       : Win32_DWORD;
        biClrImportant  : Win32_DWORD;
      end record;

  type Win32_BITMAP is
    record
      bmType       : Win32_LONG;
      bmWidth      : Win32_LONG;
      bmHeight     : Win32_LONG;
      bmWidthBytes : Win32_LONG;
      bmPlanes     : Win32_WORD;
      bmBitsPixel  : Win32_WORD;
      bmBits       : Win32_LPVOID;
    end record;

  type Win32_LPRECT             is access all Win32_RECT;
  type Win32_LPCREATESTRUCT     is access all Win32_CREATESTRUCT;
  type Win32_LPPOINT            is access all Win32_POINT;
  type Win32_LPLOGBRUSH         is access all Win32_LOGBRUSH;
  type Win32_LPMSG              is access all Win32_MSG;
  type Win32_LPCOLORREF         is access all Win32_COLORREF;
  type Win32_LPLOGFONT          is access all Win32_LOGFONT;
  type Win32_LPBITMAPINFOHEADER is access all Win32_BITMAPINFOHEADER;
  type Win32_LPBITMAP           is access all Win32_BITMAP;

  type Win32_CHOOSEFONT is
    record
      lStructSize       : Win32_DWORD;
      hwndOwner         : Win32_HWND      := System.Null_Address;
      hDC               : Win32_HDC       := System.Null_Address;
      lpLogFont         : Win32_LPLOGFONT;
      iPointSize        : Win32_INT       := 0;
      Flags             : Win32_DWORD     := CF_SCREENFONTS or
                                             CF_FORCEFONTEXIST or    
                                             CF_INITTOLOGFONTSTRUCT;
      rgbColors         : Win32_COLORREF  := 0;
      lCustData         : Win32_LPARAM    := 0;
      lpfnHook          : Win32_HOOKPROC  := null;
      lpTemplateName    : Win32_LPCSTR    := null;
      hInstance         : Win32_HINSTANCE := System.Null_Address;
      lpszStyle         : Win32_LPSTR     := null;
      nFontType         : Win32_WORD      := 0;
      MISSING_ALIGNMENT : Win32_WORD      := 0;
      nSizeMin          : Win32_INT       := 0;
      nSizeMax          : Win32_INT       := 0;
    end record;

  type Win32_CHOOSECOLOR is
    record
        lStructSize    : Win32_DWORD;
        hwndOwner      : Win32_HWND       := System.Null_Address;
        hInstance      : Win32_HWND       := System.Null_Address;
        rgbResult      : Win32_COLORREF   := 0;
        lpCustColors   : Win32_LPCOLORREF;
        Flags          : Win32_DWORD      := CC_RGBINIT;
        lCustData      : Win32_LPARAM     := 0;
        lpfnHook       : Win32_HOOKPROC   := null;
        lpTemplateName : Win32_LPCSTR     := null;
    end record;

  type Win32_OPENFILENAME is
    record
      lStructSize       : Win32_DWORD;
      hWndOwner         : Win32_HWND      := System.Null_Address;
      hInstance         : Win32_HINSTANCE := System.Null_Address;
      lpstrFilter       : Win32_LPCSTR    := null;
      lpstrCustomFilter : Win32_LPSTR     := null;
      nMaxCustFilter    : Win32_DWORD     := 0;
      nFilterIndex      : Win32_DWORD     := 1;
      lpstrFile         : Win32_LPSTR;
      nMaxFile          : Win32_DWORD;
      lpstrFileTitle    : Win32_LPSTR     := null;
      nMaxFileTitle     : Win32_DWORD     := 0;
      lpstrInitialDir   : Win32_LPCSTR    := null;
      lpstrTitle        : Win32_LPCSTR;
      Flags             : Win32_DWORD;
      nFileOffset       : Win32_WORD;
      nFileExtension    : Win32_WORD;
      lpstrDefExt       : Win32_LPCSTR;
      lCustData         : Win32_LPARAM    := 0;
      lpfnHook          : Win32_HOOKPROC  := null;
      lpTemplateName    : Win32_LPCSTR    := null;
    end record;

  ----------------------------------------------------------------------------
  --  Dummy variables for unused results from Win32 functions.
  ----------------------------------------------------------------------------

  Bool_Dummy : Win32_BOOL;
  Long_Dummy : Win32_LONG;

  ----------------------------------------------------------------------------
  --  The start of the range of Windows message numbers used for commands.
  ----------------------------------------------------------------------------

  WM_USER  : constant := 16#0400#;

  ----------------------------------------------------------------------------
  --  Assorted type conversions
  ----------------------------------------------------------------------------

  function To_Handle       is new Ada.Unchecked_Conversion
                                            (Integer,System.Address);
  function To_LPCSTR       is new Ada.Unchecked_Conversion
                                            (Integer,Win32_LPCSTR);
  function To_Integer      is new Ada.Unchecked_Conversion
                                            (System.Address,Integer);
  function To_WPARAM       is new Ada.Unchecked_Conversion
                                            (System.Address,Win32_WPARAM);
  function To_HDC          is new Ada.Unchecked_Conversion
                                            (Win32_WPARAM,Win32_HDC);
  function To_LONG         is new Ada.Unchecked_Conversion
                                            (System.Address,Win32_LONG);
  function To_LONG         is new Ada.Unchecked_Conversion
                                            (Win32_WNDPROC, Win32_LONG);
  function To_CREATESTRUCT is new Ada.Unchecked_Conversion
                                            (Win32_LPARAM,
                                             Win32_LPCREATESTRUCT);
  function To_BMIH         is new Ada.Unchecked_Conversion
                                            (System.Address,
                                             Win32_LPBITMAPINFOHEADER);

  function To_LPSTR  (S : Win32_String) return Win32_LPSTR;
  function To_LPCSTR (S : Win32_String) return Win32_LPCSTR;
  function To_LPARAM (S : Win32_String) return Win32_LPARAM;
  function To_String (S : Win32_String) return String;
  function To_Array  (S : String)       return Win32_String;

  ----------------------------------------------------------------------------
  --  Other utility functions
  ----------------------------------------------------------------------------

  function  Message_Box (Message : String;
                         Title   : String;
                         Flags   : Win32_UINT)    return Integer;

  function  Create_Font (Font    : Font_Type)     return Win32_HFONT;
  function  Set_Font    (Font    : Font_Type)     return Win32_LOGFONT;
  function  Get_Font    (Font    : Win32_LOGFONT) return Font_Type;

  function  MakePoint   (Value   : Win32_LPARAM)  return Win32_POINTS;

  function  RGB         (Colour  : Colour_Type)   return Win32_COLORREF;

  ----------------------------------------------------------------------------
  --  Imports from Win32 and RTS libraries
  ----------------------------------------------------------------------------

  function AppendMenu          (hMenu      : Win32_HMENU;
                                uFlags     : Win32_UINT;
                                uIDNewItem : Win32_UINT;
                                lpNewItem  : Win32_LPCSTR)
                                     return Win32_BOOL;
  function BeginPaint          (hWnd    : Win32_HWND;
                                lpPaint : access Win32_PAINTSTRUCT)
                                     return Win32_HDC;
  function CallWindowProc      (lpPrevWndFunc : Win32_LONG;
                                hWnd          : Win32_HWND;
                                Msg           : Win32_UINT;
                                wParam        : Win32_WPARAM;
                                lParam        : Win32_LPARAM)
                                     return Win32_LONG;
  function CheckMenuItem       (hMenu        : Win32_HMENU;
                                uIDCheckItem : Win32_UINT;
                                uCheck       : Win32_UINT)
                                     return Win32_DWORD;
  function ChooseColor         (lpcc : access Win32_CHOOSECOLOR)
                                     return Win32_BOOL;
  function ChooseFont          (lpcf : access Win32_CHOOSEFONT)
                                     return Win32_BOOL;
  function CreateBrushIndirect (lplb : Win32_LPLOGBRUSH)
                                     return Win32_HBRUSH;
  function CreateCompatibleDC  (hdc : Win32_HDC)
                                     return Win32_HDC;
  function CreateDC            (lpszDriver : Win32_LPCSTR;
                                lpszDevice : Win32_LPCSTR;
                                lpszOutput : Win32_LPCSTR;
                                lpInitData : Win32_LPVOID)
                                     return Win32_HDC;
  function CreateDIBitmap      (hdc        : Win32_HDC;
                                lpbmih     : Win32_LPBITMAPINFOHEADER;
                                dwInit     : Win32_DWORD;
                                lpvBits    : Win32_LPVOID;
                                lpbmi      : Win32_LPVOID;
                                fnColorUse : Win32_UINT)
                                     return Win32_HBITMAP;
  function CreateDIBitmap      (lplb : Win32_LPLOGBRUSH)
                                     return Win32_HBRUSH;
  function CreateFontIndirect  (lplf : access Win32_LOGFONT)
                                     return Win32_HFONT;
  function CreateMenu                return Win32_HMENU;
  function CreatePen           (fnPenStyle : Win32_INT;
                                nWidth     : Win32_INT;
                                clrref     : Win32_COLORREF)
                                     return Win32_HPEN;
  function CreateSolidBrush    (clrref : Win32_COLORREF)
                                     return Win32_HBRUSH;
  function CreateWindowEx      (dwExStyle    : Win32_DWORD;
                                lpClassName  : Win32_LPCSTR;
                                lpWindowName : Win32_LPCSTR;
                                dwStyle      : Win32_DWORD;
                                X            : Win32_INT;
                                Y            : Win32_INT;
                                nWidth       : Win32_INT;
                                nHeight      : Win32_INT;
                                hWndParent   : Win32_HWND;
                                hMenu        : Win32_HMENU;
                                hInstance    : Win32_HINSTANCE;
                                lpParam      : Win32_LPVOID)
                                     return Win32_HWND;
  function DefWindowProc       (hWnd   : Win32_HWND;
                                Msg    : Win32_UINT;
                                wParam : Win32_WPARAM;
                                lParam : Win32_LPARAM)
                                     return Win32_LONG;
  function DeleteDC            (hdc : Win32_HDC)
                                     return Win32_BOOL;
  function DeleteObject        (hgdiobj : Win32_HGDIOBJ)
                                     return Win32_BOOL;
  function DestroyWindow       (hWnd : Win32_HWND)
                                     return Win32_BOOL;
  function DispatchMessage     (lpMsg : Win32_LPMSG)
                                     return Win32_LONG;
  function DPTOLP              (hdc      : Win32_HDC;
                                lpPoints : access Win32_POINT;
                                nCount   : Win32_INT)
                                     return Win32_BOOL;
  function DrawMenuBar         (hWnd : Win32_HWND)
                                     return Win32_BOOL;
  function DrawText            (hDC      : Win32_HDC;
                                lpString : Win32_LPCSTR;
                                nCount   : Win32_INT;
                                lpRect   : Win32_LPRECT;
                                uFormat  : Win32_UINT)
                                     return Win32_INT;
  function Ellipse             (hdc         : Win32_HDC;
                                nLeftRect   : Win32_INT;
                                nTopRect    : Win32_INT;
                                nRightRect  : Win32_INT;
                                nBottomRect : Win32_INT)
                                     return Win32_BOOL;
  function EnableMenuItem      (hMenu         : Win32_HMENU;
                                uIDEnableItem : Win32_UINT;
                                uEnable       : Win32_UINT)
                                     return Win32_BOOL;
  function EnableWindow        (hWnd    : Win32_HWND;
                                bEnable : Win32_BOOL)
                                     return Win32_BOOL;
  function EndPaint            (hWnd    : Win32_HWND;
                                lpPaint : access Win32_PAINTSTRUCT)
                                     return Win32_BOOL;
  function EnumChildWindows    (hWndParent : Win32_HWND;
                                lpEnumFunc : Win32_WNDENUMPROC;
                                lParam     : Win32_LPARAM)
                                     return Win32_BOOL;
  function EnumThreadWindows   (dwThreadId : Win32_DWORD;
                                lpfn       : Win32_WNDENUMPROC;
                                lParam     : Win32_LPARAM)
                                     return Win32_BOOL;
  function FillRect            (hDC  : Win32_HDC;
                                lprc : Win32_LPRECT;
                                hbr  : Win32_HBRUSH)
                                     return Win32_INT;
  function Get_hInstance             return Win32_HINSTANCE;
  function Get_hPrevInstance         return Win32_HINSTANCE;
  function GetActiveWindow           return Win32_HWND;
  function GetClientRect       (hWnd   : Win32_HWND;
                                lpRect : Win32_LPRECT)
                                     return Win32_BOOL;
  function GetCurrentThreadId        return Win32_DWORD;
  function GetDC               (hWnd : Win32_HWND)
                                     return Win32_HDC;
  function GetDeviceCaps       (hdc         : Win32_HDC;
                                iCapability : Win32_INT)
                                     return Win32_INT;
  function GetFocus                  return Win32_HWND;
  function GetMapMode          (hdc : Win32_HDC)
                                     return Win32_INT;
  function GetMenu             (hWnd : Win32_HWND)
                                     return Win32_HMENU;
  function GetMenuState        (hMenu  : Win32_HMENU;
                                uId    : Win32_UINT;
                                uFlags : Win32_UINT)
                                     return Win32_UINT;
  function GetMenuString       (hMenu     : Win32_HMENU;
                                uIDItem   : Win32_UINT;
                                lpString  : Win32_LPSTR;
                                nMaxCount : Win32_INT;
                                uFlag     : Win32_UINT)
                                     return Win32_INT;
  function GetObject           (hgdiobj   : Win32_HGDIOBJ;
                                cbBuffer  : Win32_INT;
                                lpvObject : Win32_LPVOID)
                                     return Win32_INT;
  function GetOpenFileName     (lpofn : access Win32_OPENFILENAME)
                                     return Win32_BOOL;
  function GetParent           (hWnd : Win32_HWND)
                                     return Win32_HWND;
  function GetSaveFileName     (lpofn : access Win32_OPENFILENAME)
                                     return Win32_BOOL;
  function GetStockObject      (fnObject : Win32_INT)
                                     return Win32_HGDIOBJ;
  function GetSystemMetrics    (nIndex : Win32_INT)
                                     return Win32_INT;
  function GetWindow           (hWnd : Win32_HWND;
                                uCmd : Win32_UINT)
                                     return Win32_HWND;
  function GetWindowLong       (hWnd   : Win32_HWND;
                                nIndex : Win32_INT)
                                     return Win32_LONG;
  function GetWindowRect       (hWnd   : Win32_HWND;
                                lpRect : Win32_LPRECT)
                                     return Win32_BOOL;
  function InvalidateRect      (hWnd   : Win32_HWND;
                                lpRect : Win32_LPRECT;
                                bErase : Win32_BOOL)
                                     return Win32_BOOL;
  function IsDialogMessage     (hDlg  : Win32_HWND;
                                lpMsg : access Win32_MSG)
                                     return Win32_BOOL;
  function IsWindow            (hWnd : Win32_HWND)
                                     return Win32_BOOL;
  function IsWindowEnabled     (hWnd : Win32_HWND)
                                     return Win32_BOOL;
  function IsWindowVisible     (hWnd : Win32_HWND)
                                     return Win32_BOOL;
  function LineTo              (hdc  : Win32_HDC;
                                xEnd : Win32_INT;
                                yEnd : Win32_INT)
                                     return Win32_BOOL;
  function LoadCursor          (hInstance    : Win32_HINSTANCE;
                                lpCursorName : Win32_LPCSTR)
                                     return Win32_HCURSOR;
  function LoadIcon            (hInstance  : Win32_HINSTANCE;
                                lpIconName : Win32_LPCSTR)
                                     return Win32_HICON;
  function MessageBox          (hWnd      : Win32_HWND;
                                lpText    : Win32_LPCSTR;
                                lpCaption : Win32_LPCSTR;
                                uType     : Win32_UINT)
                                     return Win32_INT;
  function ModifyMenu          (hMnu       : Win32_HMENU;
                                uPosition  : Win32_UINT;
                                uFlags     : Win32_UINT;
                                uIDNewItem : Win32_UINT;
                                lpNewItem  : Win32_LPCSTR)
                                     return Win32_BOOL;
  function MoveToEx            (hdc     : Win32_HDC;
                                X       : Win32_INT;
                                Y       : Win32_INT;
                                lpPoint : Win32_LPPOINT)
                                     return Win32_BOOL;
  function PeekMessage         (lpMsg         : access Win32_MSG;
                                hWnd          : Win32_HWND;
                                wMsgFilterMin : Win32_UINT;
                                wMsgFilterMax : Win32_UINT;
                                wRemoveMsg    : Win32_UINT)
                                     return Win32_BOOL;
  function PlaySound           (pszSound : Win32_LPCSTR;
                                hmod     : Win32_HMODULE;
                                fdwSound : Win32_DWORD)
                                     return Win32_BOOL;
  function Polygon             (hdc      : Win32_HDC;
                                lpPoints : Win32_LPPOINT;
                                nCount   : Win32_INT)
                                     return Win32_BOOL;
  function Polyline            (hdc     : Win32_HDC;
                                lppt    : Win32_LPPOINT;
                                cPoints : Win32_INT)
                                     return Win32_BOOL;
  function Rectangle           (hdc         : Win32_HDC;
                                nLeftRect   : Win32_INT;
                                nTopRect    : Win32_INT;
                                nRightRect  : Win32_INT;
                                nBottomRect : Win32_INT)
                                     return Win32_BOOL;
  function RegisterClass       (lpWndClass : access Win32_WNDCLASS)
                                     return Win32_ATOM;
  function ReleaseCapture            return Win32_BOOL;
  function ReleaseDC           (hWnd : Win32_HWND;
                                hDC  : Win32_HDC)
                                     return Win32_INT;
  function RoundRect           (hdc            : Win32_HDC;
                                nLeftRect      : Win32_INT;
                                nTopRect       : Win32_INT;
                                nRightRect     : Win32_INT;
                                nBottomRect    : Win32_INT;
                                nEllipseWidth  : Win32_INT;
                                nEllipseHeight : Win32_INT)
                                     return Win32_BOOL;
  function SelectObject        (hdc     : Win32_HDC;
                                hgdiobj : Win32_HGDIOBJ)
                                     return Win32_HGDIOBJ;
  function SendMessage         (hWnd   : Win32_HWND;
                                Msg    : Win32_UINT;
                                wParam : Win32_WPARAM;
                                lParam : Win32_LPARAM)
                                     return Win32_LONG;
  function SetActiveWindow     (hWnd : Win32_HWND)
                                     return Win32_HWND;
  function SetBkMode           (hdc      : Win32_HDC;
                                fnBkMode : Win32_INT)
                                     return Win32_INT;
  function SetCapture          (hWnd : Win32_HWND)
                                     return Win32_HWND;
  function SetFocus            (hWnd : Win32_HWND)
                                     return Win32_HWND;
  function SetForegroundWindow (hWnd : Win32_HWND)
                                     return Win32_BOOL;
  function SetMapMode          (hdc       : Win32_HDC;
                                fnmapMode : Win32_INT)
                                     return Win32_INT;
  function SetMenu             (hWnd  : Win32_HWND;
                                hMenu : Win32_HMENU)
                                     return Win32_BOOL;
  function SetWindowLong       (hWnd      : Win32_HWND;
                                nIndex    : Win32_INT;
                                dwNewLong : Win32_LONG)
                                     return Win32_LONG;
  function SetWindowPos        (hWnd            : Win32_HWND;
                                hWndInsertAfter : Win32_HWND;
                                X               : Win32_INT;
                                Y               : Win32_INT;
                                cx              : Win32_INT;
                                cy              : Win32_INT;
                                uFlags          : Win32_UINT)
                                     return Win32_BOOL;
  function ShowWindow          (hWnd     : Win32_HWND;
                                nCmdShow : Win32_INT)
                                     return Win32_BOOL;
  function StretchBlt          (hdcDest      : Win32_HDC;
                                nXOriginDest : Win32_INT;
                                nYOriginDest : Win32_INT;
                                nWidthDest   : Win32_INT;
                                nHeightDest  : Win32_INT;
                                hdcSrc       : Win32_HDC;
                                nXOriginSrc  : Win32_INT;
                                nYOriginSrc  : Win32_INT;
                                nWidthSrc    : Win32_INT;
                                nHeightSrc   : Win32_INT;
                                dwRop        : Win32_DWORD := 16#CC0020#)
                                     return Win32_BOOL;
  function TranslateMessage    (lpMsg : Win32_LPMSG)
                                     return Win32_BOOL;
  function UpdateWindow        (hWnd : Win32_HWND)
                                     return Win32_BOOL;

private     -- mappings to external libraries

  pragma Import (Stdcall, AppendMenu,           "AppendMenuA");
  pragma Import (Stdcall, BeginPaint,           "BeginPaint");
  pragma Import (Stdcall, CallWindowProc,       "CallWindowProcA");
  pragma Import (Stdcall, CheckMenuItem,        "CheckMenuItem");
  pragma Import (Stdcall, ChooseColor,          "ChooseColorA");
  pragma Import (Stdcall, ChooseFont,           "ChooseFontA");
  pragma Import (Stdcall, CreateBrushIndirect,  "CreateBrushIndirect");
  pragma Import (Stdcall, CreateCompatibleDC,   "CreateCompatibleDC");
  pragma Import (Stdcall, CreateDC,             "CreateDCA");
  pragma Import (Stdcall, CreateDIBitmap,       "CreateDIBitmap");
  pragma Import (Stdcall, CreateFontIndirect,   "CreateFontIndirectA");
  pragma Import (Stdcall, CreateMenu,           "CreateMenu");
  pragma Import (Stdcall, CreatePen,            "CreatePen");
  pragma Import (Stdcall, CreateSolidBrush,     "CreateSolidBrush");
  pragma Import (Stdcall, CreateWindowEx,       "CreateWindowExA");
  pragma Import (Stdcall, DefWindowProc,        "DefWindowProcA");
  pragma Import (Stdcall, DeleteDC,             "DeleteDC");
  pragma Import (Stdcall, DeleteObject,         "DeleteObject");
  pragma Import (Stdcall, DestroyWindow,        "DestroyWindow");
  pragma Import (Stdcall, DispatchMessage,      "DispatchMessageA");
  pragma Import (Stdcall, DPtoLP,               "DPtoLP");
  pragma Import (Stdcall, DrawMenuBar,          "DrawMenuBar");
  pragma Import (Stdcall, DrawText,             "DrawTextA");
  pragma Import (Stdcall, Ellipse,              "Ellipse");
  pragma Import (Stdcall, EnableMenuItem,       "EnableMenuItem");
  pragma Import (Stdcall, EnableWindow,         "EnableWindow");
  pragma Import (Stdcall, EndPaint,             "EndPaint");
  pragma Import (Stdcall, EnumChildWindows,     "EnumChildWindows");
  pragma Import (Stdcall, EnumThreadWindows,    "EnumThreadWindows");
  pragma Import (Stdcall, FillRect,             "FillRect");
  pragma Import (C      , Get_hInstance ,       "rts_get_hInstance");
  pragma Import (C      , Get_hPrevInstance ,   "rts_get_hPrevInstance");
  pragma Import (Stdcall, GetActiveWindow,      "GetActiveWindow");
  pragma Import (Stdcall, GetClientRect,        "GetClientRect");
  pragma Import (Stdcall, GetCurrentThreadId,   "GetCurrentThreadId");
  pragma Import (Stdcall, GetDC,                "GetDC");
  pragma Import (Stdcall, GetDeviceCaps,        "GetDeviceCaps");
  pragma Import (Stdcall, GetFocus,             "GetFocus");
  pragma Import (Stdcall, GetMapMode,           "GetMapMode");
  pragma Import (Stdcall, GetMenu,              "GetMenu");
  pragma Import (Stdcall, GetMenuState,         "GetMenuState");
  pragma Import (Stdcall, GetMenuString,        "GetMenuStringA");
  pragma Import (Stdcall, GetObject,            "GetObjectA");
  pragma Import (Stdcall, GetOpenFileName,      "GetOpenFileNameA");
  pragma Import (Stdcall, GetParent,            "GetParent");
  pragma Import (Stdcall, GetSaveFileName,      "GetSaveFileNameA");
  pragma Import (Stdcall, GetStockObject,       "GetStockObject");
  pragma Import (Stdcall, GetSystemMetrics,     "GetSystemMetrics");
  pragma Import (Stdcall, GetWindow,            "GetWindow");
  pragma Import (Stdcall, GetWindowLong,        "GetWindowLongA");
  pragma Import (Stdcall, GetWindowRect,        "GetWindowRect");
  pragma Import (Stdcall, InvalidateRect,       "InvalidateRect");
  pragma Import (Stdcall, IsDialogMessage,      "IsDialogMessage");
  pragma Import (Stdcall, IsWindow,             "IsWindow");
  pragma Import (Stdcall, IsWindowEnabled,      "IsWindowEnabled");
  pragma Import (Stdcall, IsWindowVisible,      "IsWindowVisible");
  pragma Import (Stdcall, LineTo,               "LineTo");
  pragma Import (Stdcall, LoadCursor,           "LoadCursorA");
  pragma Import (Stdcall, LoadIcon,             "LoadIconA");
  pragma Import (Stdcall, MessageBox,           "MessageBoxA");
  pragma Import (Stdcall, ModifyMenu,           "ModifyMenuA");
  pragma Import (Stdcall, MoveToEx,             "MoveToEx");
  pragma Import (Stdcall, PeekMessage,          "PeekMessageA");
  pragma Import (Stdcall, PlaySound,            "PlaySoundA");
  pragma Import (Stdcall, Polygon,              "Polygon");
  pragma Import (Stdcall, Polyline,             "Polyline");
  pragma Import (Stdcall, Rectangle,            "Rectangle");
  pragma Import (Stdcall, RegisterClass,        "RegisterClassA");
  pragma Import (Stdcall, ReleaseCapture,       "ReleaseCapture");
  pragma Import (Stdcall, ReleaseDC,            "ReleaseDC");
  pragma Import (Stdcall, RoundRect,            "RoundRect");
  pragma Import (Stdcall, SelectObject,         "SelectObject");
  pragma Import (Stdcall, SendMessage,          "SendMessageA");
  pragma Import (Stdcall, SetActiveWindow,      "SetActiveWindow");
  pragma Import (Stdcall, SetBkMode,            "SetBkMode");
  pragma Import (Stdcall, SetCapture,           "SetCapture");
  pragma Import (Stdcall, SetFocus,             "SetFocus");
  pragma Import (Stdcall, SetForegroundWindow,  "SetForegroundWindow");
  pragma Import (Stdcall, SetMapMode,           "SetMapMode");
  pragma Import (Stdcall, SetMenu,              "SetMenu");
  pragma Import (Stdcall, SetWindowLong,        "SetWindowLongA");
  pragma Import (Stdcall, SetWindowPos,         "SetWindowPos");
  pragma Import (Stdcall, ShowWindow,           "ShowWindow");
  pragma Import (Stdcall, StretchBlt,           "StretchBlt");
  pragma Import (Stdcall, TranslateMessage,     "TranslateMessage");
  pragma Import (Stdcall, UpdateWindow,         "UpdateWindow");

end JEWL.Win32_Interface;
------------------------------------------------------------------------------
--                                                                          --
--                         J E W L . W I N D O W S                          --
--                                                                          --
--   The body of the GUI package for use with Microsoft Windows.            --
--                                                                          --
--   All window types contain a Controlled_Type object, which contains a    --
--   pointer to a Reference_Counted_Type. The type Window_Internals is      --
--   derived from Reference_Counted_Type to provide the internal data       --
--   structures needed for all windows, and further derivations are made    --
--   for specific types of window. Since Reference_Counted_Type is a        --
--   controlled type, all derivations must occur at library level, so       --
--   this is done in a separate non-generic private package which also      --
--   contains other implementation details used by this package body.       --
--   Even so, this is a large package!                                      --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-windows.adb 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-windows.adb $
-- Revision 1.7  2007/01/08 17:00:00  JE
-- * Fixed linker options in JEWL.Win32_Interface to accommodate changes to GNAT
--   GPL 2006 compiler (thanks to John McCormick for this)
-- * Added delay in message loop to avoid the appearance of hogging 100% of CPU
--   time
--
-- Revision 1.6  2001/11/02 16:00:00  JE
-- * Fixed canvas bug when saving an empty canvas
-- * Restore with no prior save now acts as erase
-- * Removed redundant variable declaration in Image function
--
-- Revision 1.5  2001/08/22 15:00:00  JE
-- * Minor bugfix to Get_Text for combo boxes
-- * Minor changes to documentation (including new example involving dialogs)
--
-- Revision 1.4  2001/01/25 09:00:00  je
-- Changes visible to the user:
--
-- * Added support for drawing bitmaps on canvases (Draw_Image operations
--   and new type Image_Type)
-- * Added Play_Sound
-- * Added several new operations on all windows: Get_Origin, Get_Width,
--   Get_Height, Set_Origin, Set_Size and Focus
-- * Added several functions giving screen and window dimensions: Screen_Width,
--   Screen_Height, Frame_Width, Frame_Height, Dialog_Width, Dialog_Height and
--   Menu_Height
-- * Canvases can now handle keyboard events: new constructor and Key_Code added
-- * Added procedure Play_Sound
-- * Operations "+" and "-" added for Point_Type
-- * Pens can now be zero pixels wide
-- * The absolute origin of a frame can now have be specified when the frame
--   is created
-- * Added new File_Dialog operations Add_Filter and Set_Directory
-- * Added Get_Line renames to JEWL.IO for compatibility with Ada.Text_IO
-- * Added all the Get(File,Item) operations mentioned in documentation but
--   unaccountably missing :-(
-- * Documentation updated to reflect the above changes
-- * HTML versions of public package specifications added with links from
--   main documentation pages
--
-- Other internal changes:
--
-- * Canvas fonts, pens etc. now use JEWL.Reference_Counted_Type rather than
--   reinventing this particular wheel, as do images
-- * Various minor code formatting changes: some code reordered for clarity,
--   some comments added or amended,
-- * Changes introduced in 1.2 to support GNAT 3.10 have been reversed, since
--   GNAT 3.10 still couldn't compile this code correctly... ;-(
--
-- Outstanding issues:
--
-- * Optimisation breaks the code (workaround: don't optimise)
--
-- Revision 1.3  2000/07/07 12:00:00  je
-- * JEWL.Simple_Windows added; JEWL.IO modified to use JEWL.Simple_Windows.
-- * JEWL.IO bug fix: Put_Line to file wrote newline to standard output
--   instead of to the file (thanks to Jeff Carter for pointing this out).
-- * Panels fixed so that mouse clicks are passed on correctly to subwindows.
-- * Memos fixed so that tabs are handled properly.
-- * Password feature added to editboxes.
-- * Minor typos fixed in comments within the package sources.
-- * Documentation corrected and updated following comments from Moti Ben-Ari
--   and Don Overheu.
--
-- Revision 1.2  2000/04/18 20:00:00  je
-- * Minor code changes to enable compilation by GNAT 3.10
-- * Minor documentation errors corrected
-- * Some redundant "with" clauses removed
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

with JEWL.Window_Implementation; use JEWL.Window_Implementation;
with JEWL.Message_Handling;      use JEWL.Message_Handling;
with JEWL.Canvas_Implementation; use JEWL.Canvas_Implementation;
with JEWL.Win32_Interface;       use JEWL.Win32_Interface;

with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Tags;                   use Ada.Tags;
with Ada.Streams.Stream_IO;      use Ada.Streams.Stream_IO;
with System;

package body JEWL.Windows is

  use type System.Address;
  use type Ada.Streams.Stream_Element_Offset;
  use type Win32_BOOL, Win32_LONG, Win32_WORD,
           Win32_UINT, Win32_SIZE, Win32_DWORD;

  ----------------------------------------------------------------------------
  --  Win32 window class names
  ----------------------------------------------------------------------------

  Frame_Class  : constant Win32_String := To_Array("JEWL.Windows.Frame");
  Dialog_Class : constant Win32_String := To_Array("JEWL.Windows.Dialog");
  Canvas_Class : constant Win32_String := To_Array("JEWL.Windows.Canvas");

  ----------------------------------------------------------------------------
  --  End-of-line string
  ----------------------------------------------------------------------------

  EOL : constant String := ASCII.CR & ASCII.LF;

  ----------------------------------------------------------------------------
  --
  --              M I S C E L L A N E O U S   R O U T I N E S
  --
  ----------------------------------------------------------------------------
  --
  --  Show_Error: display a message box with an OK button and stop sign.
  --
  procedure Show_Error (Text  : in String;
                        Title : in String := "Error") is
    I : Integer;
  begin
    I := Message_Box (Text, Title, MB_OK+MB_ICONSTOP);
  end Show_Error;

  ----------------------------------------------------------------------------
  --
  --  Show_Query: display a message box with Yes/No buttons and a question
  --              mark.
  --
  function Show_Query (Text  : in String;
                       Title : in String := "Query") return Boolean is
  begin
    return Message_Box (Text, Title, MB_YESNO+MB_ICONQUESTION) = IDYES;
  end Show_Query;

  ----------------------------------------------------------------------------
  --
  --  Show_Message: display a message box with an OK button and an
  --                information sign.
  --
  procedure Show_Message (Text  : in String;
                          Title : in String := "Message") is
    I : Integer;
  begin
    I := Message_Box (Text, Title, MB_OK+MB_ICONINFORMATION);
  end Show_Message;

  ----------------------------------------------------------------------------
  --
  --  Play_Sound: play a sound held in a wave file.
  --
  procedure Play_Sound (Sound : in String) is
  begin
    Bool_Dummy := PlaySound (To_LPCSTR(To_Array(Sound)), System.Null_Address,
                             SND_FILENAME + SND_NODEFAULT + SND_ASYNC);
  end Play_Sound;

  ----------------------------------------------------------------------------
  --
  --  Screen_Width: get width of display screen in pixels.
  --
  function Screen_Width return Natural is
  begin
    return Natural(GetSystemMetrics(SM_CXSCREEN));
  end Screen_Width;

  ----------------------------------------------------------------------------
  --
  --  Screen_Height: get height of display screen in pixels.
  --
  function Screen_Height return Natural is
  begin
    return Natural(GetSystemMetrics(SM_CYSCREEN));
  end Screen_Height;

  ----------------------------------------------------------------------------
  --
  --                I N T E R N A L   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Get_Internals: check that a window has been initialised and return a
  --                 pointer to its Window_Internals structure, or raise an
  --                 Invalid_Window exception. Additional parameters are
  --                 used to generate a meaningful message to accompany
  --                 the exception.
  --
  function Get_Internals (Window    : in Window_Type'Class;
                          Operation : in String) return Window_Ptr is
  begin
    if Window.Internals.Pointer = null then
      Raise_Exception (Invalid_Window'Identity,
                       External_Tag(Window'Tag) &
                       ": window not initialised in call to " & Operation);
    end if;
    return Window_Ptr(Window.Internals.Pointer);
  end Get_Internals;

  ----------------------------------------------------------------------------
  --
  --  Get_Internals: check that a common dialog has been initialised and
  --                 return a pointer to its Common_Dialog_Internals, or
  --                 raise an Invalid_Window exception. Additional parameters
  --                 are used to generate a meaningful message to accompany
  --                 the exception.
  --
  function Get_Internals (Dialog    : in Common_Dialog_Type'Class;
                          Operation : in String) return Common_Dialog_Ptr is
  begin
    if Dialog.Internals.Pointer = null then
      Raise_Exception (Invalid_Window'Identity,
                       External_Tag(Dialog'Tag) &
                       ": dialog not initialised in call to " & Operation);
    end if;
    return Common_Dialog_Ptr(Dialog.Internals.Pointer);
  end Get_Internals;

  ----------------------------------------------------------------------------
  --
  --  Add: add an object to the end of a canvas drawing list and invalidate
  --       the canvas window so it will be repainted. The second parameter
  --       enables the actual operation name to be passed to Get_Internals.
  --
  procedure Add (Canvas    : in Canvas_Type;
                 Operation : in String;
                 Object    : in Canvas_Object_Ptr) is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, Operation));
  begin
    C.Monitor.Add (Object);
    Bool_Dummy := InvalidateRect (C.Handle, null, 1);
  end;

  ----------------------------------------------------------------------------
  --
  --  Create_Child: create a child window with specified characteristics.
  --                The last parameter enables the actual operation name
  --                to be passed to Get_Internals.
  --
  procedure Create_Child (Window    : in out Window_Type'Class;
                          Parent    : in Container_Type'Class;
                          Class     : in String;
                          Title     : in String;
                          XStyle    : in Win32_DWORD;
                          Style     : in Win32_DWORD;
                          Origin    : in Point_Type;
                          Width     : in Integer;
                          Height    : in Integer;
                          Font      : in Font_Type;
                          ID        : in Integer;
                          Operation : in String) is
    P : Container_Ptr := Container_Ptr (Get_Internals (Parent, Operation));
    X : Window_Ptr := Get_Internals (Window, Operation);
    T : Integer := Origin.Y;
    L : Integer := Origin.X;
    H : Integer := Height;
    W : Integer := Width;
    B : Boolean;
    C : Win32_String := To_Array(Class);
    N : Win32_String := To_Array(Title);
  begin
    -- Fill in links to parent and siblings

    X.Parent := P;
    if P.Last = null then
      P.First := Window.Internals;
    else
      P.Last.Next := Window.Internals;
    end if;
    P.Last := X;

    -- Fill in the command code associated with this window

    X.Action := ID;

    -- Fill in the window dimensions

    X.Top    := Origin.Y;
    X.Left   := Origin.X;
    X.Height := Height;
    X.Width  := Width;

    -- Calculate the actual window dimensions (the dimensions given may be
    -- relative to the parent window)

    Get_Actual_Bounds (P.Handle, T, L, W, H, B);

    -- Ask the message loop task to create the child window

    Message_Loop.Create_Child (X, P, C, N, XStyle, Style, T, L, W, H);

    -- Create the font, or use the parent font if no font name is given

    if Font.Length > 0 then
      Set_Font (Window, Font);
    else
      X.Font := System.Null_Address;
    end if;

  end Create_Child;

  ----------------------------------------------------------------------------
  --
  --                   I M A G E   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Image: load a bitmap image from a specified file.
  --
  function Image (Name : String) return Image_Type is
    subtype Offset   is Ada.Streams.Stream_Element_Offset;
    subtype Elements is Ada.Streams.Stream_Element_Array;

    Image   : Image_Type;
    File    : Ada.Streams.Stream_IO.File_Type;
    Stream  : Ada.Streams.Stream_IO.Stream_Access;
    Header  : Win32_BITMAPFILEHEADER;
    Info    : aliased Win32_BITMAPINFOHEADER;
    Colours : Offset;
    Bytes   : Offset;
    Bitmap  : Win32_HBITMAP;
    Pointer : Image_Ptr;
  begin
    Image.Internals.Pointer := null;

    -- Create a stream to read the bitmap file

    Ada.Streams.Stream_IO.Open (File, Name => Name, Mode => In_File);
    Stream := Ada.Streams.Stream_IO.Stream (File);

    -- Read and check the file header

    Win32_BITMAPFILEHEADER'Read (Stream, Header);
    if Header.bfType /= BITMAP_MAGIC then
      raise Data_Error;
    end if;

    -- Read and check the bitmap info header

    Win32_BITMAPINFOHEADER'Read (Stream, Info);
    if Info.biSize /= Info'Size/Win32_BYTE'Size or Info.biPlanes /= 1 then
      raise Data_Error;
    end if;

    -- Calculate no. of colour table entries following the info header

    if Info.biClrUsed /= 0 then
      Colours := Offset(Info.biClrUsed);
    elsif Info.biBitCount <= 8 then
      Colours := Offset(2 ** Integer(Info.biBitCount));
    elsif Info.biCompression = BI_BITFIELDS then
      Colours := 3;
    else
      Colours := 0;
    end if;

    -- Calculate size of bitmap data

    Bytes := Offset(Info.biSizeImage);
    if Bytes = 0 then
      Bytes := (Offset(Info.biWidth) * Offset(Info.biBitCount) + 31) / 32
               * Offset(Info.biHeight);
    end if;

    -- Process the rest of the file

    declare
      C : Elements (1 .. Colours * 4 + Offset(Info.biSize));
      D : Elements (1 .. Offset(Bytes));
      E : Offset;
      P : Win32_BITMAPINFOHEADER;     for P'Address use C(1)'Address;
      H : Win32_HDC;
    begin

      -- Copy the bitmap info header into the header block

      P := Info;

      -- Read the colour table into the header block

      Ada.Streams.Read (Stream.all, C (Offset(Info.biSize)+1 .. C'Last), E);
      if E /= C'Length then
        raise Data_Error;
      end if;

      -- Read the rest of the file into the data block

      Ada.Streams.Read (Stream.all, D, E);
      if E /= D'Length then
        raise Data_Error;
      end if;

      -- Get a device context for the display

      H := CreateDC (To_LPCSTR(To_Array("DISPLAY")),
                     null, null, System.Null_Address); 
      if H = System.Null_Address then
        raise Data_Error;
      end if;

      -- Create the bitmap using the display context

      Bitmap := CreateDIBitmap (H, Info'Unchecked_Access, CBM_INIT,
                                D(1)'Address, C(1)'Address, DIB_RGB_COLORS);
      if Bitmap = System.Null_Address then
        raise Data_Error;
      end if;
    end;

    -- Fill in image structure

    Pointer := new Image_Internals;

    Pointer.Image  := Bitmap;
    Pointer.Width  := Natural(Info.biWidth);
    Pointer.Height := Natural(Info.biHeight);

    Image.Internals.Pointer := Reference_Counted_Ptr(Pointer);

    Close (File);
    return Image;

  exception
    when others =>
      Close (File);
      return Image;

  end Image;

  ----------------------------------------------------------------------------
  --
  --  Valid: get the width of the specified image.
  --
  function Valid (Image : Image_Type) return Boolean is
  begin
    return Image.Internals.Pointer /= null;
  end Valid;

  ----------------------------------------------------------------------------
  --
  --  Width: get the width of the specified image.
  --
  function Width (Image : Image_Type) return Natural is
  begin
    if Valid(Image) then
      return Image_Internals(Image.Internals.Pointer.all).Width;
    else
      return 0;
    end if;
  end Width;

  ----------------------------------------------------------------------------
  --
  --  Height: get the height of the specified image.
  --
  function Height (Image : Image_Type) return Natural is
  begin
    if Valid(Image) then
      return Image_Internals(Image.Internals.Pointer.all).Height;
    else
      return 0;
    end if;
  end Height;

  ----------------------------------------------------------------------------
  --
  --                  W I N D O W   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Show: make a window visible or invisible, bringing visible windows to
  --        the foreground.
  --
  procedure Show (Window  : in Window_Type;
                  Visible : in Boolean := True) is
    P : Window_Ptr := Get_Internals (Window, "Show");
  begin
    if Visible then
      Bool_Dummy := ShowWindow(P.Handle,SW_SHOW);
      Bool_Dummy := SetForegroundWindow (P.Handle);
    else
      Bool_Dummy := ShowWindow(P.Handle,SW_HIDE);
    end if;
  end Show;

  ----------------------------------------------------------------------------
  --
  --  Hide: use Show (above) to hide a window.
  --
  procedure Hide (Window : in Window_Type) is
    P : Window_Ptr := Get_Internals (Window, "Hide");
  begin
    Show (Window, False);
  end Hide;

  ----------------------------------------------------------------------------
  --
  --  Focus: give the input focus to the specified window.
  --
  procedure Focus (Window : in Window_Type) is
    P : Window_Ptr := Get_Internals (Window, "Focus");
  begin
    Message_Loop.Set_Focus (P.Handle);
  end Focus;

  ----------------------------------------------------------------------------
  --
  --  Visible: test if a window is visible.
  --
  function Visible (Window : Window_Type) return Boolean is
    P : Window_Ptr := Get_Internals (Window, "Visible");
  begin
    return IsWindowVisible(P.Handle) /= 0;
  end Visible;

  ----------------------------------------------------------------------------
  --
  --  Get_Origin: get the coordinates of a window's top left corner.
  --
  function Get_Origin (Window : Window_Type) return Point_Type is
    P : Window_Ptr := Get_Internals (Window, "Visible");
    R : aliased Win32_RECT;
  begin
    Bool_Dummy := GetWindowRect (P.Handle, R'Unchecked_Access);
    return (Integer(R.Left),Integer(R.Top));
  end Get_Origin;

  ----------------------------------------------------------------------------
  --
  --  Get_Width: get the width of a window.
  --
  function Get_Width (Window : Window_Type) return Natural is
    P : Window_Ptr := Get_Internals (Window, "Get_Width");
    R : aliased Win32_RECT;
  begin
    Bool_Dummy := GetWindowRect (P.Handle, R'Unchecked_Access);
    return Natural (R.Right - R.Left);
  end Get_Width;

  ----------------------------------------------------------------------------
  --
  --  Get_Height: get the height of a window.
  --
  function Get_Height (Window : Window_Type) return Natural is
    P : Window_Ptr := Get_Internals (Window, "Get_Height");
    R : aliased Win32_RECT;
  begin
    Bool_Dummy := GetWindowRect (P.Handle, R'Unchecked_Access);
    return Natural (R.Bottom - R.Top);
  end Get_Height;

  ----------------------------------------------------------------------------
  --
  --  Set_Origin: set the coordinates of a window's top left corner.
  --
  procedure Set_Origin (Window : in Window_Type;
                        Origin : in Point_Type) is
    P : Window_Ptr := Get_Internals (Window, "Set_Origin");
  begin
    Bool_Dummy := SetWindowPos (P.Handle, System.Null_Address,
                                Win32_INT(Origin.X), Win32_INT(Origin.Y),
                                0, 0, SWP_NOZORDER + SWP_NOSIZE);
  end Set_Origin;

  ----------------------------------------------------------------------------
  --
  --  Set_Size: set the width and height of a window.
  --
  procedure Set_Size (Window : in Window_Type;
                      Width  : in Natural := 0;
                      Height : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Window, "Set_Size");
    W : Natural := Width;
    H : Natural := Height;
  begin
    if Width = 0 then
      W := Get_Width (Window);
    end if;
    if Height = 0 then
      H := Get_Height (Window);
    end if;
    Bool_Dummy := SetWindowPos (P.Handle, System.Null_Address, 0, 0,
                                Win32_INT(W), Win32_INT(H),
                                SWP_NOZORDER + SWP_NOMOVE);
  end Set_Size;

  ----------------------------------------------------------------------------
  --
  --  Set_Font: change the font associated with a window and invalidate it
  --            so that it will be repainted.
  --
  procedure Set_Font (Window : in Window_Type;
                      Font   : in Font_Type) is
    P : Window_Ptr := Get_Internals (Window, "Set_Font");
  begin
    if Font.Length > 0 then
      if P.Font /= System.Null_Address then
        Bool_Dummy := DeleteObject (P.Font);
      end if;
      P.Font := Create_Font (Font);
      Long_Dummy := SendMessage (P.Handle, WM_SETFONT, To_WPARAM(P.Font), 0);
      Bool_Dummy := InvalidateRect (P.Handle, null, 1);
    end if;
  end Set_Font;

  ----------------------------------------------------------------------------
  --
  --  Get_Font: build a Font_Type structure for a window's current font.
  --
  function Get_Font (Window : Window_Type) return Font_Type is
    P : Window_Ptr := Get_Internals (Window, "Get_Font");
    I : Win32_INT;
    F : Win32_LOGFONT;
  begin
    while P.Font = System.Null_Address loop
      P := Window_Ptr(P.Parent);
      exit when P = null;
    end loop;
    if P = null or else P.Font = System.Null_Address then
      return Default_Font;
    else
      I := GetObject (P.Font, Win32_LOGFONT'Size/Win32_BYTE'Size, F'Address);
      return Get_Font (F);
    end if;
  end Get_Font;

  ----------------------------------------------------------------------------
  --
  --                   F R A M E   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Frame: construct a frame with the specified characteristics.
  --
  function Frame (Origin  : Point_Type;
                  Width   : Positive;
                  Height  : Positive;
                  Title   : String;
                  Command : Command_Type;
                  Font    : Font_Type := Default_Font) return Frame_Type is
    W : Frame_Type;
    M : Main_Window_Ptr := new Main_Window_Internals;
    P : Window_Ptr      := Window_Ptr(M);
    T : Win32_String    := To_Array(Title);
  begin
    -- Set up the Window_Internals structure for a window with default
    -- placement

    W.Internals.Pointer := Reference_Counted_Ptr(P);
    P.Action := Command_Type'Pos(Command);
    P.Top    := Origin.Y;
    P.Left   := Origin.X;
    P.Width  := Width;
    P.Height := Height;

    -- Ask the message loop to create a top-level window

    Message_Loop.Create_Window (M, Frame_Class, T,
                                WS_EX_CLIENTEDGE or WS_EX_APPWINDOW,
                                WS_OVERLAPPEDWINDOW,
                                True);

    -- Set the font now that the frame exists

    if Font.Length > 0 then
      Set_Font (W, Font);
    else
      Set_Font (W, Default_Font);
    end if;

    -- Bring the window to the front and return the window object

    Bool_Dummy := SetForegroundWindow(P.Handle);
    return W;
  end Frame;

  ----------------------------------------------------------------------------
  --
  --  Frame: construct a frame with the specified characteristics and
  --         default placement.
  --
  function Frame (Width   : Positive;
                  Height  : Positive;
                  Title   : String;
                  Command : Command_Type;
                  Font    : Font_Type := Default_Font) return Frame_Type is
  begin
    return Frame ((Integer(CW_USEDEFAULT),Integer(CW_USEDEFAULT)),
                  Width, Height, Title, Command, Font);
  end Frame;

  ----------------------------------------------------------------------------
  --
  --  Close: close and destroy a frame.
  --
  procedure Close (Frame : in Frame_Type) is
    P : Window_Ptr := Get_Internals (Frame, "Close");
  begin
    if IsWindow(P.Handle) /= 0 then
      Message_Loop.Destroy_Window (P.Handle);
    end if;
  end Close;

  ----------------------------------------------------------------------------
  --
  --  Valid: test if a frame is valid (i.e. if the window exists).
  --
  function Valid (Frame : Frame_Type) return Boolean is
    P : Window_Ptr := Window_Ptr(Frame.Internals.Pointer);
  begin
    return P /= null and then
           IsWindow(P.Handle) /= 0;
  end Valid;

  ----------------------------------------------------------------------------
  --
  --  Frame_Width: return the width of a frame's border.
  --
  function Frame_Width return Natural is
  begin
    return Natural
           ((GetSystemMetrics(SM_CXFRAME) + GetSystemMetrics(SM_CXEDGE)) * 2);
  end Frame_Width;

  ----------------------------------------------------------------------------
  --
  --  Frame_Height: return the height of a frame's border.
  --
  function Frame_Height return Natural is
  begin
    return Natural
           ((GetSystemMetrics(SM_CYFRAME) + GetSystemMetrics(SM_CYEDGE)) * 2 +
            GetSystemMetrics(SM_CYCAPTION));
  end Frame_Height;

  ----------------------------------------------------------------------------
  --
  --  Next_Command: ask the info monitor for the next command.
  --
  function Next_Command return Command_Type is
    Cmd : Natural;
  begin
    Window_Info.Get_Command (Cmd);
    return Command_Type'Val(Cmd);
  end Next_Command;

  ----------------------------------------------------------------------------
  --
  --  Command_Ready: ask the info monitor if there is a command pending.
  --
  function Command_Ready return Boolean is
  begin
    return Window_Info.Test_Command;
  end Command_Ready;

  ----------------------------------------------------------------------------
  --
  --                  D I A L O G   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Dialog: create a top-level dialog window.
  --
  function Dialog (Width   : Positive;
                   Height  : Positive;
                   Title   : String;
                   Command : Command_Type;
                   Font    : Font_Type := Default_Font) return Dialog_Type is
    W : Dialog_Type;
    X : Integer := Integer(GetSystemMetrics(SM_CXSCREEN)) / 2;
    Y : Integer := Integer(GetSystemMetrics(SM_CYSCREEN)) / 2;
    M : Main_Window_Ptr  := new Main_Window_Internals;
    P : Window_Ptr       := Window_Ptr(M);
    T : Win32_String := To_Array(Title);
  begin
    -- Set up the Window_Internals structure for a centred window

    W.Internals.Pointer := Reference_Counted_Ptr(P);
    P.Action := Command_Type'Pos(Command);
    P.Top    := Y - Height/2;
    P.Left   := X - Width/2;
    P.Width  := Width;
    P.Height := Height;

    -- Ask the message loop to create a hidden top-level window

    Message_Loop.Create_Window (M, Dialog_Class, T,
                                0, WS_DLGFRAME or WS_SYSMENU,
                                False);

    -- Set the font now that the dialog exists

    if Font.Length > 0 then
      Set_Font (W, Font);
    else
      Set_Font (W, Default_Font);
    end if;

    -- Return the window object

    return W;
  end Dialog;

  ----------------------------------------------------------------------------
  --
  --  Execute: run a dialog until it issues a command. Note that dialogs
  --           are hidden rather than destroyed so that closing a dialog
  --           them won't make any attached controls disappear.
  --
  function Execute (Dialog  : in Dialog_Type) return Command_Type is
    D : Win32_HWND;
    C : Natural;
    P : Window_Ptr := Get_Internals (Dialog, "Execute");
  begin
    -- Record this window as the currently active dialog

    D := P.Handle;
    Window_Info.Get_Dialog (D);

    -- Make the window visible and bring it to the foreground

    Bool_Dummy := ShowWindow (P.Handle, SW_SHOW);
    Bool_Dummy := SetForegroundWindow (P.Handle);

    -- Wait for a command (which must be from this dialog, as dialog
    -- windows disable all other windows belonging to the application)

    Window_Info.Get_Command (C);

    -- Restore the original active dialog setting

    Window_Info.Get_Dialog (D);

    -- Hide the dialog window and return the command code

    Bool_Dummy := ShowWindow (P.Handle, SW_HIDE);
    return Command_Type'Val(C);
  end Execute;

  ----------------------------------------------------------------------------
  --
  --  Dialog_Width: return the width of a dialog's border.
  --
  function Dialog_Width return Natural is
  begin
    return Natural (GetSystemMetrics(SM_CXDLGFRAME) * 2);
  end Dialog_Width;

  ----------------------------------------------------------------------------
  --
  --  Dialog_Height: return the height of a dialog's border.
  --
  function Dialog_Height return Natural is
  begin
    return Natural (GetSystemMetrics(SM_CYDLGFRAME) * 2 +
                    GetSystemMetrics(SM_CYCAPTION));
  end Dialog_Height;

  ----------------------------------------------------------------------------
  --
  --                   P A N E L   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Panel: create a panel (which is actually a Windows groupbox if it has
  --         a title, or a static control with an etched border if not).
  --
  function Panel (Parent : Container_Type'Class;
                  Origin : Point_Type;
                  Width  : Integer;
                  Height : Integer;
                  Title  : String := "";
                  Font   : Font_Type := Parent_Font) return Panel_Type is
    W : Panel_Type;
    C : String(1..6);
    S : Win32_DWORD := WS_GROUP;
    P : Container_Ptr := new Container_Internals;
  begin
    -- Choose the actual window class and style

    if Title = "" then
      C := "static";
      S := S or SS_ETCHEDFRAME;
    else
      C := "button";
      S := S or BS_GROUPBOX;
    end if;

    -- Create the window and return it

    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, C, Title, WS_EX_CONTROLPARENT, S,
                  Origin, Width, Height, Font, -1, "Panel");
    P.WndProc := GetWindowLong (P.Handle, GWL_WNDPROC);
    Long_Dummy := SetWindowLong (P.Handle, GWL_WNDPROC,
                                 To_LONG(Panel_Proc'Access));
    return W;
  end Panel;

  ----------------------------------------------------------------------------
  --
  --                    M E N U   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Menu: create a menu attached to a frame.
  --
  function Menu (Parent : Frame_Type'Class;
                 Text   : String) return Menu_Type is
    M : Menu_Type;
    H : Win32_HMENU;
    P : Window_Ptr := Get_Internals (Parent, "Menu");
    W : Window_Ptr := new Window_Internals;
    T : Win32_String := To_Array(Text);
  begin
    -- Get the frame's menu bar (and create it if it doesn't exist)

    H := GetMenu (P.Handle);
    if H = System.Null_Address then
      H := CreateMenu;
      Bool_Dummy := SetMenu (P.Handle, H);
    end if;

    -- Create a new menu and attach it to the menu bar

    W.Handle := CreateMenu;
    Bool_Dummy := AppendMenu(H, MF_POPUP, To_WPARAM(W.Handle), To_LPCSTR(T));

    -- Redraw the menu bar and return the menu object

    Bool_Dummy := DrawMenuBar (P.Handle);
    M.Internals.Pointer := Reference_Counted_Ptr(W);
    return M;
  end Menu;

  ----------------------------------------------------------------------------
  --
  --  Menu: create a menu attached to another menu.
  --
  function Menu (Parent : Menu_Type'Class;
                 Text   : String) return Menu_Type is
    M : Menu_Type;
    P : Window_Ptr := Get_Internals (Parent, "Menu");
    W : Window_Ptr := new Window_Internals;
    H : Win32_HWND := P.Handle;
    T : Win32_String := To_Array(Text);
  begin
    -- Create a new submenu and attach it to the parent menu

    W.Handle := CreateMenu;
    Bool_Dummy := AppendMenu(H, MF_POPUP, To_WPARAM(W.Handle), To_LPCSTR(T));

    -- Find the enclosing top-level window and redraw its menu bar

    while GetParent(H) /= System.Null_Address loop
      H := GetParent(H);
    end loop;
    Bool_Dummy := DrawMenuBar(H);

    -- Return the menu object

    M.Internals.Pointer := Reference_Counted_Ptr(W);
    return M;
  end Menu;

  ----------------------------------------------------------------------------
  --
  --  Menu_Height: return the height of a menubar.
  --
  function Menu_Height return Natural is
  begin
    return Natural (GetSystemMetrics(SM_CYMENU));
  end Menu_Height;

  ----------------------------------------------------------------------------
  --
  --                 C O N T R O L   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Enable: enable or disable a control.
  --
  procedure Enable (Control : in Control_Type;
                    Enabled : in Boolean := True) is
    P : Window_Ptr := Get_Internals (Control, "Enable");
  begin
    Bool_Dummy := EnableWindow (P.Handle, Win32_BOOL(Boolean'Pos(Enabled)));
  end Enable;

  ----------------------------------------------------------------------------
  --
  --  Disable: use Enable (above) to disable a control.
  --
  procedure Disable (Control : in Control_Type) is
    P : Window_Ptr := Get_Internals (Control, "Disable");
  begin
    Enable (Control_Type'Class(Control), False);
  end Disable;

  ----------------------------------------------------------------------------
  --
  --  Enabled: test if a control is enabled.
  --
  function Enabled (Control : Control_Type) return Boolean is
    P : Window_Ptr := Get_Internals (Control, "Enabled");
  begin
    return IsWindowEnabled(P.Handle) /= 0;
  end Enabled;

  ----------------------------------------------------------------------------
  --
  --            T E X T   C O N T R O L   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Get_Length: get the length of the text in a text control.
  --
  function Get_Length (Control : Text_Control_Type) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Length");
  begin
    return Natural(SendMessage(P.Handle, WM_GETTEXTLENGTH, 0, 0));
  end Get_Length;

  ----------------------------------------------------------------------------
  --
  --  Get_Text: get the text from a text control.
  --
  function Get_Text (Control : Text_Control_Type) return String is
    P : Window_Ptr := Get_Internals (Control, "Get_Text");
    L : Natural;
  begin
    declare
      A : Win32_String(1..Win32_SIZE(Get_Length(Control)+1)) := (others => ' ');
    begin
      L := Natural(SendMessage(P.Handle, WM_GETTEXT,
                               Win32_WPARAM(A'Length), To_LPARAM(A)));
      return To_String(A);
    end;
  end Get_Text;

  ----------------------------------------------------------------------------
  --
  --  Get_Text: get the text from a text control into a fixed-length
  --            string variable.
  --
  procedure Get_Text (Control : in  Text_Control_Type;
                      Text    : out String;
                      Length  : out Natural) is
    S : constant String := Get_Text (Control);
  begin
    if S'Length > Text'Length then
      Text := S(S'First..S'First+Text'Length-1);
      Length := Text'Length;
    else
      Text(Text'First..Text'First+S'Length-1) := S;
      Length := S'Length;
    end if;
  end Get_Text;

  ----------------------------------------------------------------------------
  --
  --  Set_Text: store the specified text in a text control.
  --
  procedure Set_Text (Control : in  Text_Control_Type;
                      Text    : in  String) is
    P : Window_Ptr := Get_Internals (Control, "Set_Text");
    T : Win32_String := To_Array(Text);
  begin
    Long_Dummy := SendMessage (P.Handle, WM_SETTEXT, 0, To_LPARAM(T));
  end Set_Text;

  ----------------------------------------------------------------------------
  --
  --                 B U T T O N   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Button: create a button as specified.
  --
  function Button (Parent  : Container_Type'Class;
                   Origin  : Point_Type;
                   Width   : Integer;
                   Height  : Integer;
                   Text    : String;
                   Command : Command_Type;
                   Default : Boolean := False;
                   Font    : Font_Type := Parent_Font) return Button_Type is
    W : Button_Type;
    P : Window_Ptr  := new Window_Internals;
    S : Win32_DWORD := WS_TABSTOP or WS_GROUP;
  begin
    if Default then
      S := S or BS_DEFPUSHBUTTON;
    else
      S := S or BS_PUSHBUTTON;
    end if;
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, "button", Text,
                  0, S, Origin, Width, Height, Font,
                  Command_Type'Pos(Command)+WM_USER, "Button");
    return W;
  end Button;

  ----------------------------------------------------------------------------
  --
  --                  L A B E L   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Label: create a label as specified.
  --
  function Label (Parent : Container_Type'Class;
                  Origin : Point_Type;
                  Width  : Integer;
                  Height : Integer;
                  Text   : String;
                  Align  : Alignment_Type := Left;
                  Font   : Font_Type := Parent_Font) return Label_Type is
    W : Label_Type;
    P : Window_Ptr := new Window_Internals;
    S : Win32_DWORD := WS_GROUP or SS_NOPREFIX;
  begin
    if Align = Right then
      S := S or SS_RIGHT;
    elsif Align = Centre then
      S := S or SS_CENTER;
    end if;
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, "static", Text, 0, S,
                  Origin, Width, Height, Font, -1, "Label");
    return W;
  end Label;

  ----------------------------------------------------------------------------
  --
  --                E D I T B O X   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Editbox: create an editbox as specified.
  --
  function Editbox (Parent   : Container_Type'Class;
                    Origin   : Point_Type;
                    Width    : Integer;
                    Height   : Integer;
                    Text     : String := "";
                    Password : Boolean := False;
                    Font     : Font_Type := Parent_Font) return Editbox_Type is
    W : Editbox_Type;
    P : Window_Ptr := new Window_Internals;
    E : Win32_DWORD := ES_AUTOHSCROLL or WS_BORDER or WS_TABSTOP or WS_GROUP;
  begin
    if Password then
      E := E or ES_PASSWORD;
    end if;
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, "edit", Text, 0, E,
                  Origin, Width, Height, Font, -1, "Editbox");
    return W;
  end Editbox;

  ----------------------------------------------------------------------------
  --
  --  Modified: test if the user has modified the editbox since the last
  --            time this function was called.
  --
  function Modified (Editbox : Editbox_Type) return Boolean is
    P : Window_Ptr := Get_Internals(Editbox, "Modified");
    B : Boolean;
  begin
    B := SendMessage(P.Handle,EM_GETMODIFY,0,0) /= 0;
    Long_Dummy := SendMessage(P.Handle, EM_SETMODIFY, 0, 0);
    return B;
  end Modified;

  ----------------------------------------------------------------------------
  --
  --         B O O L E A N   C O N T R O L   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Get_State: test if a Boolean control is checked.
  --
  function Get_State (Control : Boolean_Control_Type) return Boolean is
    P : Window_Ptr := Get_Internals (Control, "Get_State");
  begin
    return SendMessage (P.Handle, BM_GETCHECK, 0, 0) = 1;
  end Get_State;

  ----------------------------------------------------------------------------
  --
  --  Set_State: set the state of a Boolean control as specified.
  --
  procedure Set_State (Control : in Boolean_Control_Type;
                       State   : in Boolean) is
    P : Window_Ptr := Get_Internals (Control, "Set_State");
  begin
    Long_Dummy := SendMessage (P.Handle, BM_SETCHECK,
                               Boolean'Pos(State), 0);
  end Set_State;

  ----------------------------------------------------------------------------
  --
  --                M E N U I T E M   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Menuitem: create a menuitem.
  --
  function Menuitem (Parent  : Menu_Type'Class;
                     Text    : String;
                     Command : Command_Type) return Menuitem_Type is
    M : Menuitem_Type;
    P : Window_Ptr := Get_Internals (Parent, "Menuitem");
    W : Window_Ptr := new Window_Internals;
    H : Win32_HWND := P.Handle;
    T : Win32_String := To_Array(Text);
  begin
    -- Set the command code and set the internal handle to be the parent
    -- handle (since Win32 menuitems are not real windows and do not have
    -- handles of their own)

    M.Internals.Pointer := Reference_Counted_Ptr(W);
    W.Handle := P.Handle;
    W.Action := Command_Type'Pos(Command);

    -- Add the menuitem to the parent menu

    Bool_Dummy := AppendMenu(P.Handle, MF_STRING,
                             Win32_UINT(W.Action+WM_USER),
                             To_LPCSTR(T));

    -- Find the enclosing top-level window and redraw its menu bar

    while GetParent(H) /= System.Null_Address loop
      H := GetParent(H);
    end loop;
    Bool_Dummy := DrawMenuBar(H);

    -- Return the menuitem object

    return M;
  end Menuitem;

  ----------------------------------------------------------------------------
  --
  --  Separator: create a separator for a menu.
  --
  function Separator (Parent : Menu_Type'Class) return Menuitem_Type is
    M : Menuitem_Type;
    P : Window_Ptr := Get_Internals (Parent, "Separator");
    W : Window_Ptr := new Window_Internals;
    H : Win32_HWND := P.Handle;
  begin
    -- Set the command code and set the internal handle to be the parent
    -- handle (since Win32 menuitems are not real windows and do not have
    -- handles of their own)

    M.Internals.Pointer := Reference_Counted_Ptr(W);
    W.Handle := P.Handle;
    W.Action := -1;

    -- Add the menuitem to the parent menu

    Bool_Dummy := AppendMenu(P.Handle, MF_STRING or MF_SEPARATOR, 0, null);

    -- Find the enclosing top-level window and redraw its menu bar

    while GetParent(H) /= System.Null_Address loop
      H := GetParent(H);
    end loop;
    Bool_Dummy := DrawMenuBar(H);

    -- Return the menuitem object

    return M;
  end Separator;

  ----------------------------------------------------------------------------
  --
  --  Enable: enable or disable a menu item using its command code.
  --
  procedure Enable (Control : in Menuitem_Type;
                    Enabled : in Boolean := True) is
    P : Window_Ptr := Get_Internals (Control, "Enable");
    E : Win32_UINT;
  begin
    if P.Action >= 0 then
      if Enabled then
        E := MF_BYCOMMAND or MF_ENABLED;
      else
        E := MF_BYCOMMAND or MF_GRAYED;
      end if;
      Bool_Dummy := EnableMenuItem (P.Handle,
                                    Win32_UINT(P.Action+WM_USER), E);
    end if;
  end Enable;

  ----------------------------------------------------------------------------
  --
  --  Enabled: test if a menu item is enabled using its command code.
  --
  function Enabled (Control : Menuitem_Type) return Boolean is
    P : Window_Ptr := Get_Internals (Control, "Enabled");
    S : Win32_UINT;
  begin
    if P.Action < 0 then
      return False;
    else
      S := GetMenuState (P.Handle,
                         Win32_UINT(P.Action+WM_USER), MF_BYCOMMAND);
      return (S and MF_DISABLED) = 0;
    end if;
  end Enabled;

  ----------------------------------------------------------------------------
  --
  --  Get_Length: get the length of the text in a menuitem.
  --
  function Get_Length (Control : Menuitem_Type) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Length");
  begin
    if P.Action < 0 then
      return 0;
    else
      return Natural(GetMenuString(P.Handle, Win32_UINT(P.Action+WM_USER),
                                   null, 0, MF_BYCOMMAND));
    end if;
  end Get_Length;

  ----------------------------------------------------------------------------
  --
  --  Get_Text: get the text from a menuitem.
  --
  function Get_Text (Control : Menuitem_Type) return String is
    P : Window_Ptr := Get_Internals (Control, "Get_Text");
    L : Natural;
  begin
    if P.Action < 0 then
      return "";
    else
      declare
        A : Win32_String(1..Win32_SIZE(Get_Length(Control)+1)) :=
                                                      (others => ' ');
      begin
        L := Natural(GetMenuString(P.Handle, Win32_UINT(P.Action+WM_USER),
                                   To_LPSTR(A), A'Length, MF_BYCOMMAND));
        return To_String(A);
      end;
    end if;
  end Get_Text;

  ----------------------------------------------------------------------------
  --
  --  Set_Text: store the specified text in a text control.
  --
  procedure Set_Text (Control : in  Menuitem_Type;
                      Text    : in  String) is
    P : Window_Ptr := Get_Internals (Control, "Set_Text");
    T : Win32_String := To_Array(Text);
    H : Win32_HWND;
  begin
    if P.Action >= 0 then      -- ignore menu separators
      Bool_Dummy := ModifyMenu (P.Handle, Win32_UINT(P.Action+WM_USER),
                                MF_BYCOMMAND or MF_STRING,
                                Win32_UINT(P.Action), To_LPCSTR(T));
      H := P.Handle;
      while GetParent(H) /= System.Null_Address loop
        H := GetParent(H);
      end loop;
      Bool_Dummy := DrawMenuBar(H);
    end if;
  end Set_Text;

  ----------------------------------------------------------------------------
  --
  --  Get_State: test if a menuitem is checked.
  --
  function Get_State (Control : Menuitem_Type) return Boolean is
    P : Window_Ptr := Get_Internals (Control, "Get_State");
  begin
    return (GetMenuState(P.Handle,Win32_UINT(P.Action+WM_USER),
                         MF_BYCOMMAND) and MF_CHECKED) /= 0;
  end Get_State;

  ----------------------------------------------------------------------------
  --
  --  Set_State: set the state of a menuitem as specified.
  --
  procedure Set_State (Control : in Menuitem_Type;
                       State   : in Boolean) is
    P : Window_Ptr := Get_Internals (Control, "Set_State");
    D : Win32_DWORD;
  begin
    if State then
      D := CheckMenuItem (P.Handle, Win32_UINT(P.Action+WM_USER),
                                    MF_BYCOMMAND or MF_CHECKED);
    else
      D := CheckMenuItem (P.Handle, Win32_UINT(P.Action+WM_USER),
                                    MF_BYCOMMAND or MF_UNCHECKED);
    end if;
  end Set_State;

  ----------------------------------------------------------------------------
  --
  --               C H E C K B O X   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Checkbox: create a checkbox with the specified initial state.
  --
  function Checkbox (Parent  : Container_Type'Class;
                     Origin  : Point_Type;
                     Width   : Integer;
                     Height  : Integer;
                     Text    : String;
                     Checked : Boolean := False;
                     Font    : Font_Type := Parent_Font)
                                                  return Checkbox_Type is
    W : Checkbox_Type;
    P : Window_Ptr := new Window_Internals;
  begin
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, "button", Text, 0,
                  BS_AUTOCHECKBOX or WS_TABSTOP or WS_GROUP,
                  Origin, Width, Height, Font, -1, "Checkbox");
    Set_State (W, Checked);
    return W;
  end Checkbox;

  ----------------------------------------------------------------------------
  --
  --            R A D I O B U T T O N   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Radiobutton: create a radiobutton with the specified initial state.
  --
  function Radiobutton (Parent  : Container_Type'Class;
                        Origin  : Point_Type;
                        Width   : Integer;
                        Height  : Integer;
                        Text    : String;
                        Checked : Boolean   := False;
                        Font    : Font_Type := Parent_Font)
                                                  return Radiobutton_Type is
    W : Radiobutton_Type;
    P : Window_Ptr := new Window_Internals;
  begin
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, "button", Text, 0,
                  BS_AUTORADIOBUTTON or WS_TABSTOP,
                  Origin, Width, Height, Font, -1, "Radiobutton");
    Set_State (W, Checked);
    return W;
  end Radiobutton;

  ----------------------------------------------------------------------------
  --
  --              M U L T I L I N E   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Get_Text: get the text of a specified line into a fixed-length
  --            string variable by dispatching to the appropriate
  --            Get_Text function.
  --
  procedure Get_Text  (Control : in  Multiline_Type;
                       Line    : in  Natural := 0;
                       Text    : out String;
                       Length  : out Natural) is
    S : constant String := Get_Text (Multiline_Type'Class(Control), Line);
  begin
    if S'Length > Text'Length then
      Text := S(S'First..S'First+Text'Length-1);
      Length := Text'Length;
    else
      Text(Text'First..Text'First+S'Length-1) := S;
      Length := S'Length;
    end if;
  end Get_Text;

  ----------------------------------------------------------------------------
  --
  --  Get_Actual_Line: convert a line number in a multiline control
  --                   (which may be zero or out-of-range) to an
  --                   absolute line number (internal use only).
  --
  function Get_Actual_Line (Control : in Multiline_Type'Class;
                            Line    : in Natural;
                            Name    : in String) return Natural is
    L : Natural := Line;
  begin
    if L > Get_Count(Control) then
      Raise_Exception (Constraint_Error'Identity,
                       External_Tag(Control'Tag) &
                       ": Line number out of range in " & Name);
    end if;
    if L = 0 then
      L := Get_Line(Control);
    end if;
    return L;
  end Get_Actual_Line;

  ----------------------------------------------------------------------------
  --
  --                L I S T B O X   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Listbox: create a listbox.
  --
  function Listbox (Parent : Container_Type'Class;
                    Origin : Point_Type;
                    Width  : Integer;
                    Height : Integer;
                    Font   : Font_Type := Parent_Font) return Listbox_Type is
    W : Listbox_Type;
    P : Window_Ptr := new Window_Internals;
  begin
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, "listbox", "", 0,
                  WS_HSCROLL or WS_VSCROLL or WS_BORDER or
                  WS_TABSTOP or WS_GROUP,
                  Origin, Width, Height, Font, -1, "Listbox");
    return W;
  end Listbox;

  ----------------------------------------------------------------------------
  --
  --  Get_Count: get the number of lines in the listbox.
  --
  function Get_Count (Control : Listbox_Type) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Count");
  begin
    return Natural(SendMessage(P.Handle,LB_GETCOUNT,0,0));
  end Get_Count;

  ----------------------------------------------------------------------------
  --
  --  Get_Line: get the number of the current line (0 if no line is
  --            selected).
  --
  function Get_Line (Control : Listbox_Type) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Line");
  begin
    return Natural(SendMessage(P.Handle,LB_GETCURSEL,0,0) + 1);
  end Get_Line;

  ----------------------------------------------------------------------------
  --
  --  Get_Length: get the length of the specified line (0 if no line
  --              is selected).
  --
  function Get_Length (Control : Listbox_Type;
                       Line    : Natural := 0) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Length");
    L : Natural := Get_Actual_Line (Control, Line, "Get_Length");
  begin
    if L = 0 then
      return 0;
    else
      return Natural(SendMessage(P.Handle, LB_GETTEXTLEN,
                                 Win32_WPARAM(L)-1, 0));
    end if;
  end Get_Length;

  ----------------------------------------------------------------------------
  --
  --  Get_Text: get the text of the specified line (the empty string if
  --            the current line is specified and no line is selected).
  --
  function Get_Text (Control : Listbox_Type;
                     Line    : Natural := 0) return String is
    P : Window_Ptr := Get_Internals (Control, "Get_Text");
    L : Natural := Get_Actual_Line (Control, Line, "Get_Text");
  begin
    if L = 0 then
      return "";
    else
      declare
        A : Win32_String(1..Win32_SIZE(Get_Length(Control,L)+1)) :=
                                                           (others => ' ');
      begin
        L := Natural(SendMessage(P.Handle, LB_GETTEXT,
                                 Win32_WPARAM(L)-1, To_LPARAM(A)));
        return To_String(A);
      end;
    end if;
  end Get_Text;

  ----------------------------------------------------------------------------
  --
  --  Set_Text: set the text of the specified line (delete the current
  --            line and insert its replacement).
  --
  procedure Set_Text (Control : in Listbox_Type;
                      Text    : in String;
                      Line    : in Natural := 0) is
    L : Natural := Get_Actual_Line (Control, Line, "Set_Text");
  begin
    Delete_Line (Control, L);
    Insert_Line (Control, Text, L);
  end Set_Text;

  ----------------------------------------------------------------------------
  --
  --  Select_Line: set the line number for the current selection (deselect
  --               all lines if the line number is 0).
  --
  procedure Select_Line (Control : in Listbox_Type;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Select_Line");
  begin
    if Line > Get_Count(Control) then
      Raise_Exception (Constraint_Error'Identity,
                       External_Tag(Multiline_Type'Class(Control)'Tag) &
                       ": Line number out of range in Select_Line");
    end if;
    Long_Dummy := SendMessage(P.Handle, LB_SETCURSEL,
                              Win32_WPARAM(Line)-1, 0);
  end Select_Line;

  ----------------------------------------------------------------------------
  --
  --  Append_Line: add a line containing the specified line to the end
  --               of the listbox.
  --
  procedure Append_Line (Control : in Listbox_Type;
                         Text    : in String) is
    P : Window_Ptr := Get_Internals (Control, "Append_Line");
    T : Win32_String := To_Array(Text);
  begin
    Long_Dummy := SendMessage(P.Handle, LB_ADDSTRING, 0, To_LPARAM(T));
  end Append_Line;

  ----------------------------------------------------------------------------
  --
  --  Insert_Line: insert a new line above the specified line. If the real
  --               line number is zero (no current line), append the line
  --               as above.
  --
  procedure Insert_Line (Control : in Listbox_Type;
                         Text    : in String;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Insert_Line");
    L : Natural := Get_Actual_Line (Control, Line, "Insert_Line");
    T : Win32_String := To_Array(Text);
  begin
    if L = 0 then
      Append_Line (Control, Text);
    else
      Long_Dummy := SendMessage(P.Handle, LB_INSERTSTRING,
                                Win32_WPARAM(L)-1, To_LPARAM(T));
    end if;
  end Insert_Line;

  ----------------------------------------------------------------------------
  --
  --  Delete_Line: delete the specified line.
  --
  procedure Delete_Line (Control : in Listbox_Type;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Delete_Line");
    L : Natural := Get_Actual_Line (Control, Line, "Delete_Line");
  begin
    Long_Dummy := SendMessage(P.Handle, LB_DELETESTRING,
                              Win32_WPARAM(L)-1, 0);
  end Delete_Line;

  ----------------------------------------------------------------------------
  --
  --  Delete_All: delete all lines in the listbox.
  --
  procedure Delete_All (Control : in Listbox_Type) is
    P : Window_Ptr := Get_Internals (Control, "Delete_All");
  begin
    Long_Dummy := SendMessage(P.Handle, LB_RESETCONTENT, 0, 0);
  end Delete_All;

  ----------------------------------------------------------------------------
  --
  --               C O M B O B O X   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Combobox: create a combobox.
  --
  function Combobox (Parent   : Container_Type'Class;
                     Origin   : Point_Type;
                     Width    : Integer;
                     Editable : Boolean := True;
                     Font     : Font_Type := Parent_Font)
                                                  return Combobox_Type is
    W : Combobox_Type;
    P : Window_Ptr := new Window_Internals;
    S : Win32_DWORD := CBS_AUTOHSCROLL or WS_GROUP;
  begin
    if Editable then
      S := S or CBS_DROPDOWN;
    else
      S := S or CBS_DROPDOWNLIST;
    end if;
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, "combobox", "", 0,
                  S or WS_HSCROLL or WS_VSCROLL or WS_BORDER or WS_TABSTOP,
                  Origin, Width, 120, Font, -1, "Combobox");
    return W;
  end Combobox;

  ----------------------------------------------------------------------------
  --
  --  Get_Count: get the number of lines in the combobox.
  --
  function Get_Count (Control : Combobox_Type) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Count");
  begin
    return Natural(SendMessage(P.Handle,CB_GETCOUNT,0,0));
  end Get_Count;

  ----------------------------------------------------------------------------
  --
  --  Get_Line: get the number of the current line (0 if no line is
  --            selected, or if the text in the editbox part of the
  --            control is not a string selected from the listbox
  --            part).
  --
  function Get_Line (Control : Combobox_Type) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Line");
  begin
    return Natural(SendMessage(P.Handle,CB_GETCURSEL,0,0) + 1);
  end Get_Line;

  ----------------------------------------------------------------------------
  --
  --  Get_Length: get the length of the specified line (0 if no line
  --              is selected).
  --
  function Get_Length (Control : Combobox_Type;
                       Line    : Natural := 0) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Length");
    L : Natural := Get_Actual_Line (Control, Line, "Get_Length");
  begin
    if L = 0 then
      return Natural(SendMessage(P.Handle, WM_GETTEXTLENGTH, 0, 0));
    else
      return Natural(SendMessage(P.Handle, CB_GETLBTEXTLEN,
                                 Win32_WPARAM(L)-1, 0));
    end if;
  end Get_Length;

  ----------------------------------------------------------------------------
  --
  --  Get_Text: get the text of the specified line (the text of the editbox
  --            part of the control if the line number is 0).
  --
  function Get_Text (Control : Combobox_Type;
                     Line    : Natural := 0) return String is
    P : Window_Ptr := Get_Internals (Control, "Get_Text");
    L : Natural := Get_Actual_Line (Control, Line, "Get_Text");
  begin
    declare
      A : Win32_String(1..Win32_SIZE(Get_Length(Control,L)+1)) :=
                                                      (others => ' ');
    begin
      if L = 0 then
        Long_Dummy := SendMessage(P.Handle, WM_GETTEXT,
                                  Win32_WPARAM(A'Length), To_LPARAM(A));
      else
        Long_Dummy := SendMessage(P.Handle, CB_GETLBTEXT,
                                  Win32_WPARAM(L-1), To_LPARAM(A));
      end if;
      return To_String(A);
    end;
  end Get_Text;

  ----------------------------------------------------------------------------
  --
  --  Set_Text: set the text of the specified line (delete the current
  --            line and insert its replacement).
  --
  procedure Set_Text (Control : in Combobox_Type;
                      Text    : in String;
                      Line    : in Natural := 0) is
    L : Natural := Get_Actual_Line (Control, Line, "Set_Text");
  begin
    Delete_Line (Control, L);
    if L > Get_Count(Control) then
      L := 0;
    end if;
    Insert_Line (Control, Text, L);
  end Set_Text;

  ----------------------------------------------------------------------------
  --
  --  Select_Line: set the line number for the current selection (deselect
  --               all lines if the line number is 0).
  --
  procedure Select_Line (Control : in Combobox_Type;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Select_Line");
    L : Natural := Get_Actual_Line (Control, Line, "Select_Line");
  begin
    Long_Dummy := SendMessage(P.Handle, CB_SETCURSEL,
                              Win32_WPARAM(Line)-1, 0);
  end Select_Line;

  ----------------------------------------------------------------------------
  --
  --  Append_Line: add a line containing the specified line to the end
  --               of the listbox part of the combobox.
  --
  procedure Append_Line (Control : in Combobox_Type;
                         Text    : in String) is
    P : Window_Ptr := Get_Internals (Control, "Append_Line");
    T : Win32_String := To_Array(Text);
  begin
    Long_Dummy := SendMessage(P.Handle, CB_ADDSTRING, 0, To_LPARAM(T));
  end Append_Line;

  ----------------------------------------------------------------------------
  --
  --  Insert_Line: insert a new line above the specified line. If the real
  --
  procedure Insert_Line (Control : in Combobox_Type;
                         Text    : in String;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Insert_Line");
    L : Natural := Get_Actual_Line (Control, Line, "Insert_Line");
    T : Win32_String := To_Array(Text);
  begin
    if L = 0 then
      Append_Line (Control, Text);
    else
      Long_Dummy := SendMessage(P.Handle, CB_INSERTSTRING,
                                Win32_WPARAM(L)-1, To_LPARAM(T));
    end if;
  end Insert_Line;

  ----------------------------------------------------------------------------
  --
  --  Delete_Line: delete the specified line.
  --
  procedure Delete_Line (Control : in Combobox_Type;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Delete_Line");
    L : Natural := Get_Actual_Line (Control, Line, "Delete_Line");
  begin
    if L = 0 then
      Select_Line (Control);
    else
      Long_Dummy := SendMessage(P.Handle, CB_DELETESTRING,
                                Win32_WPARAM(L)-1, 0);
    end if;
  end Delete_Line;

  ----------------------------------------------------------------------------
  --
  --  Delete_All: delete all lines in the combobox.
  --
  procedure Delete_All (Control : in Combobox_Type) is
    P : Window_Ptr := Get_Internals (Control, "Delete_All");
  begin
    Long_Dummy := SendMessage(P.Handle, CB_RESETCONTENT, 0, 0);
  end Delete_All;

  ----------------------------------------------------------------------------
  --
  --                   M E M O   O P E R A T I O N S
  --
  --  Memos are slightly peculiar because Windows always reports them as
  --  having at least one line, even when they're completely empty. I've
  --  decided that a blank last line won't count as a line -- a CR/LF at
  --  the end of a line is part of the line it ends, and only lines with
  --  characters in them should count. So there.
  --
  ----------------------------------------------------------------------------
  --
  --  Last_Line: returns character index of start of last line (for internal
  --             use only).
  --
  function Last_Line (Memo : in Win32_HWND) return Win32_LONG is
    L : Win32_LONG;
  begin
    L := SendMessage (Memo, EM_GETLINECOUNT, 0, 0);
    return SendMessage (Memo, EM_LINEINDEX, Win32_WPARAM(L-1), 0);
  end Last_Line;

  ----------------------------------------------------------------------------
  --
  --  Length: returns length of memo text (for internal use only).
  --
  function Length (Memo : in Win32_HWND) return Win32_LONG is
  begin
    return SendMessage (Memo, WM_GETTEXTLENGTH, 0, 0);
  end Length;

  ----------------------------------------------------------------------------
  --
  --  Memo: create a memo control as specified.
  --
  function Memo (Parent : Container_Type'Class;
                 Origin : Point_Type;
                 Width  : Integer;
                 Height : Integer;
                 Font   : Font_Type := Parent_Font) return Memo_Type is
    W : Memo_Type;
    P : Window_Ptr := new Window_Internals;
  begin
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, "edit", "", WS_EX_CLIENTEDGE,
                  ES_MULTILINE or ES_WANTRETURN or ES_NOHIDESEL or
                  ES_AUTOHSCROLL or ES_AUTOVSCROLL or
                  WS_HSCROLL or WS_VSCROLL or
                  WS_TABSTOP or WS_GROUP,
                  Origin, Width, Height, Font, -1, "Memo");
    P.WndProc := GetWindowLong (P.Handle, GWL_WNDPROC);
    Long_Dummy := SetWindowLong (P.Handle, GWL_WNDPROC,
                                 To_LONG(Memo_Proc'Access));
    return W;
  end Memo;

  ----------------------------------------------------------------------------
  --
  --  Get_Column: find the column number where the caret is positioned.
  --
  function Get_Column (Memo : Memo_Type) return Natural is
    P : Window_Ptr := Get_Internals (Memo, "Get_Column");
    S : Integer;
    L : Win32_LONG;
  begin
    Long_Dummy := SendMessage (P.Handle, EM_GETSEL,
                               To_WPARAM(S'Address), 0);
    L := SendMessage (P.Handle, EM_LINEFROMCHAR,
                      Win32_WPARAM(S), 0);
    L := SendMessage(P.Handle, EM_LINEINDEX, Win32_WPARAM(L), 0);
    return S - Integer(L) + 1;
  end Get_Column;

  ----------------------------------------------------------------------------
  --
  --  Modified: test if the user has modified the memo since the last
  --            time this function was called.
  --
  function Modified (Memo : Memo_Type) return Boolean is
    P : Window_Ptr := Get_Internals(Memo, "Modified");
    B : Boolean;
  begin
    B := SendMessage(P.Handle,EM_GETMODIFY,0,0) /= 0;
    Long_Dummy := SendMessage(P.Handle, EM_SETMODIFY, 0, 0);
    return B;
  end Modified;

  ----------------------------------------------------------------------------
  --
  --  Cut_Selection: cut the current selection to the clipboard.
  --
  procedure Cut_Selection (Memo : in Memo_Type) is
    P : Window_Ptr := Get_Internals (Memo, "Cut_Selection");
  begin
    Long_Dummy := SendMessage (P.Handle, WM_CUT, 0, 0);
  end Cut_Selection;

  ----------------------------------------------------------------------------
  --
  --  Copy_Selection: copy the current selection to the clipboard.
  --
  procedure Copy_Selection (Memo : in Memo_Type) is
    P : Window_Ptr := Get_Internals (Memo, "Copy_Selection");
  begin
    Long_Dummy := SendMessage (P.Handle, WM_COPY, 0, 0);
  end Copy_Selection;

  ----------------------------------------------------------------------------
  --
  --  Paste_Selection: paste the clipboard over the current selection.
  --
  procedure Paste_Selection (Memo : in Memo_Type) is
    P : Window_Ptr := Get_Internals (Memo, "Paste_Selection");
  begin
    Long_Dummy := SendMessage (P.Handle, WM_PASTE, 0, 0);
  end Paste_Selection;

  ----------------------------------------------------------------------------
  --
  --  Undo_Change: undo the user's last change to the text of the memo.
  --
  procedure Undo_Change (Memo : in Memo_Type) is
    P : Window_Ptr := Get_Internals (Memo, "Undo_Change");
  begin
    Long_Dummy := SendMessage (P.Handle, WM_UNDO, 0, 0);
  end Undo_Change;

  ----------------------------------------------------------------------------
  --
  --  Show_Selection: scroll the memo so that the caret is in view.
  --
  procedure Show_Selection (Memo : in Memo_Type) is
    P : Window_Ptr := Get_Internals(Memo, "Show_Selection");
  begin
    Long_Dummy := SendMessage (P.Handle, EM_SCROLLCARET, 0, 0);
  end Show_Selection;

  ----------------------------------------------------------------------------
  --
  --  Get_Count: get the number of lines in the memo.
  --
  function Get_Count (Control : Memo_Type) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Count");
  begin
    return Natural(SendMessage(P.Handle, EM_GETLINECOUNT, 0, 0)) -
           Boolean'Pos(Last_Line(P.Handle) = Length(P.Handle));
  end Get_Count;

  ----------------------------------------------------------------------------
  --
  --  Get_Line: get the number of the line where the caret is positioned.
  --            Return zero if it's on a blank last line.
  --
  function Get_Line (Control : Memo_Type) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Line");
  begin
    if Last_Line(P.Handle) = Length(P.Handle) then
      return 0;
    else
      return Natural(SendMessage(P.Handle,EM_LINEFROMCHAR,-1,0)) + 1;
    end if;
  end Get_Line;

  ----------------------------------------------------------------------------
  --
  --  Get_Length: get the length of the specified line.
  --
  function Get_Length (Control : Memo_Type;
                       Line    : Natural := 0) return Natural is
    P : Window_Ptr := Get_Internals (Control, "Get_Length");
    L : Natural := Get_Actual_Line (Control, Line, "Get_Length");
    R : Win32_LONG;
  begin
    if L = 0 then
      return 0;
    else
      R := SendMessage (P.Handle, EM_LINEINDEX, Win32_WPARAM(L)-1, 0);
      return Natural(SendMessage(P.Handle,EM_LINELENGTH,Win32_WPARAM(R),0));
    end if;
  end Get_Length;

  ----------------------------------------------------------------------------
  --
  --  Get_Text: get the text of the specified line. Note that the EM_GETLINE
  --            message takes the line length in the first two bytes of the
  --            destination string, and no terminating null is copied (so
  --            the rest of the destination string must be initialised to
  --            nulls).
  --
  function Get_Text (Control : Memo_Type;
                     Line    : Natural := 0) return String is
    P : Window_Ptr := Get_Internals(Control, "Get_Text");
    L : Natural := Get_Actual_Line (Control, Line, "Get_Text");
    W : Natural;
  begin
    W := Get_Length (Control, L);
    if W = 0 then
      return "";
    else
      declare
        A : Win32_String(1..Win32_SIZE(W+1)) :=
                                     (1 => Win32_CHAR'Val(W mod 16#100#),
                                      2 => Win32_CHAR'Val(W / 16#100#),
                                      others => Win32_Char'Val(0));
      begin
        Long_Dummy := SendMessage(P.Handle, EM_GETLINE,
                                  Win32_WPARAM(L)-1, To_LPARAM(A));
        return To_String(A);
      end;
    end if;
  end Get_Text;

  ----------------------------------------------------------------------------
  --
  --  Set_Text: set the text of the specified line (select the line and
  --            replace the selection).
  --
  procedure Set_Text (Control : in Memo_Type;
                      Text    : in String;
                      Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Set_Text");
    L : Natural := Get_Actual_Line (Control, Line, "Set_Text");
    S : Win32_LONG;     -- start position (start of line)
    E : Win32_LONG;     -- end position (start of next line)
    T : Win32_String := To_Array(Text);
  begin
    if L = 0 then
      Append_Line (Control, Text);
    else
      S := SendMessage(P.Handle, EM_LINEINDEX, Win32_WPARAM(L)-1, 0);
      E := S + Win32_LONG(Get_Length(Control,L));
      Long_Dummy := SendMessage (P.Handle, EM_SETSEL,
                                 Win32_WPARAM(S), Win32_LPARAM(E));
      Long_Dummy := SendMessage (P.Handle, EM_REPLACESEL, 0,
                                 To_LPARAM(T));
    end if;
  end Set_Text;

  ----------------------------------------------------------------------------
  --
  --  Select_Line: set the line number for the caret position.
  --
  procedure Select_Line (Control : in Memo_Type;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Select_Line");
    L : Natural := Get_Actual_Line (Control, Line, "Select_Line");
    R : Win32_LONG;
  begin
    if L = 0 then
      R := Length(P.Handle);
    else
      R := SendMessage(P.Handle, EM_LINEINDEX, Win32_WPARAM(L)-1, 0);
    end if;
    Long_Dummy := SendMessage (P.Handle, EM_SETSEL,
                               Win32_WPARAM(R), Win32_LPARAM(R));
  end Select_Line;

  ----------------------------------------------------------------------------
  --
  --  Append_Line: add a line containing the specified line to the end
  --               of the memo. If the last line is not blank, add a
  --               preceding EOL to start a new line
  --
  procedure Append_Line (Control : in Memo_Type;
                         Text    : in String) is
    P : Window_Ptr := Get_Internals (Control, "Append_Line");
    C : Integer;
  begin
    C := Integer(Length(P.Handle));
    Long_Dummy := SendMessage (P.Handle, EM_SETSEL,
                               Win32_WPARAM(C), Win32_LPARAM(C));
    if Last_Line(P.Handle) = Length(P.Handle) then
      declare
        T : Win32_String := To_Array(Text);
      begin
        Long_Dummy := SendMessage (P.Handle, EM_REPLACESEL, 0, To_LPARAM(T));
      end;
    else
      declare
        T : Win32_String := To_Array(EOL & Text);
      begin
        Long_Dummy := SendMessage (P.Handle, EM_REPLACESEL, 0, To_LPARAM(T));
      end;
    end if;
  end Append_Line;

  ----------------------------------------------------------------------------
  --
  --  Insert_Line: insert a new line above the specified line. If the line
  --               number is zero, append the line as above.
  --
  procedure Insert_Line (Control : in Memo_Type;
                         Text    : in String;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Insert_Line");
    L : Natural := Get_Actual_Line (Control, Line, "Select_Line");
    T : Win32_String := To_Array(Text & EOL);
  begin
    if L = 0 then
      Append_Line (Control, Text);
    else
      Select_Line (Control, Line);
      Long_Dummy := SendMessage (P.Handle, EM_REPLACESEL, 0, To_LPARAM(T));
    end if;
  end Insert_Line;

  ----------------------------------------------------------------------------
  --
  --  Delete_Line: delete the specified line.
  --
  procedure Delete_Line (Control : in Memo_Type;
                         Line    : in Natural := 0) is
    P : Window_Ptr := Get_Internals (Control, "Delete_Line");
    L : Natural;
    S : Win32_LONG;
    E : Win32_LONG;
    N : Win32_String := To_Array("");
  begin
    L := Get_Actual_Line (Control, Line, "Delete_Line");
    if L > 0 then
      S := SendMessage(P.Handle, EM_LINEINDEX, Win32_WPARAM(L)-1, 0);
      E := SendMessage(P.Handle, EM_LINEINDEX, Win32_WPARAM(L), 0);
      if E < 0 then
        E := Length(P.Handle);
      end if;
      Long_Dummy := SendMessage (P.Handle, EM_SETSEL,
                                 Win32_WPARAM(S), Win32_LPARAM(E));
      Long_Dummy := SendMessage (P.Handle, EM_REPLACESEL, 0,
                                 To_LPARAM(N));
    end if;
  end Delete_Line;

  ----------------------------------------------------------------------------
  --
  --  Delete_All: delete all lines in the memo.
  --
  procedure Delete_All (Control : in Memo_Type) is
    P : Window_Ptr := Get_Internals (Control, "Delete_All");
    N : Win32_String := To_Array("");
  begin
    Long_Dummy := SendMessage (P.Handle, WM_SETTEXT, 0,
                               To_LPARAM(N));
  end Delete_All;

  ----------------------------------------------------------------------------
  --
  --                 C A N V A S   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Canvas: create a canvas window which does not generate a command.
  --
  function Canvas (Parent  : Container_Type'Class;
                   Origin  : Point_Type;
                   Width   : Integer;
                   Height  : Integer;
                   Font    : Font_Type := Parent_Font) return Canvas_Type is
    W : Canvas_Type;
    P : Canvas_Ptr := new Canvas_Internals;
  begin
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, To_String(Canvas_Class), "",
                  0, WS_BORDER or WS_GROUP, 
                  Origin, Width, Height, Font, -1, "Canvas");
    Set_Fill (W);
    return W;
  end Canvas;

  ----------------------------------------------------------------------------
  --
  --  Canvas: create a canvas window which generates a command when the
  --          mouse button is pressed within it.
  --
  function Canvas (Parent  : Container_Type'Class;
                   Origin  : Point_Type;
                   Width   : Integer;
                   Height  : Integer;
                   Command : Command_Type;
                   Font    : Font_Type := Parent_Font) return Canvas_Type is
    W : Canvas_Type;
    P : Canvas_Ptr := new Canvas_Internals;
  begin
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, To_String(Canvas_Class), "",
                  0, WS_BORDER or WS_GROUP,
                  Origin, Width, Height, Font,
                  Command_Type'Pos(Command), "Canvas");
    Set_Fill (W);
    return W;
  end Canvas;

  ----------------------------------------------------------------------------
  --
  --  Canvas: create a canvas window which generates a command when the
  --          mouse button or a key is pressed within it.
  --
  function Canvas (Parent   : Container_Type'Class;
                   Origin   : Point_Type;
                   Width    : Integer;
                   Height   : Integer;
                   Command  : Command_Type;
                   Keypress : Command_Type;
                   Font     : Font_Type := Parent_Font) return Canvas_Type is
    W : Canvas_Type;
    P : Canvas_Ptr := new Canvas_Internals;
  begin
    W.Internals.Pointer := Reference_Counted_Ptr(P);
    Create_Child (W, Parent, To_String(Canvas_Class), "",
                  0, WS_BORDER or WS_GROUP,
                  Origin, Width, Height, Font,
                  Command_Type'Pos(Command), "Canvas");
    P.Keypress := Command_Type'Pos(Keypress);
    Focus (W);
    Set_Fill (W);
    return W;
  end Canvas;

  ----------------------------------------------------------------------------
  --
  --  Set_Colour: ask the monitor to set the background colour.
  --
  procedure Set_Colour (Canvas : in Canvas_Type;
                        Colour : in Colour_Type := White) is
    B : Win32_COLORREF := RGB(Colour);
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "Set_Colour"));
  begin
    C.Monitor.Set_Brush (CreateSolidBrush(B));
    Bool_Dummy := InvalidateRect (C.Handle, null, 1);
  end Set_Colour;

  ----------------------------------------------------------------------------
  --
  --  Erase: ask the monitor to delete the drawing list and then redraw
  --         the window.
  --
  procedure Erase (Canvas : in Canvas_Type) is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "Erase"));
  begin
    C.Monitor.Clear;
    Bool_Dummy := InvalidateRect (C.Handle, null, 1);
  end Erase;

  ----------------------------------------------------------------------------
  --
  --  Save: ask the monitor to save the current position in the drawing list.
  --
  procedure Save (Canvas : in Canvas_Type) is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "Save"));
  begin
    C.Monitor.Save;
  end Save;

  ----------------------------------------------------------------------------
  --
  --  Restore: revert to a previously saved position in the drawing list
  --           (ignored if there is no saved position). This is safe because
  --           the list always grows unless it is erased, so the saved
  --           position will be valid until Erase is called, at which point
  --           the monitor will reset it to null.
  --
  procedure Restore (Canvas : in Canvas_Type) is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "Restore"));
  begin
    C.Monitor.Restore;
    Bool_Dummy := InvalidateRect (C.Handle, null, 1);
  end Restore;

  ----------------------------------------------------------------------------
  --
  --  Set_Font: add a font handle to the drawing list.
  --
 procedure Set_Font (Canvas    : in Canvas_Type;
                     Font      : in Font_Type) is
    P : Canvas_Object_Ptr := new Handle_Type;
    H : Win32_HFONT       := Create_Font (Font);
  begin
    Handle_Type(P.all).Handle := Handle(H);
    Add (Canvas, "Set_Font", P);
  end Set_Font;

  ----------------------------------------------------------------------------
  --
  --  Set_Pen: add a pen handle to the drawing list.
  --
  procedure Set_Pen (Canvas : in Canvas_Type;
                     Colour : in Colour_Type := Black;
                     Width  : in Natural     := 1) is
    P : Canvas_Object_Ptr := new Handle_Type;
    S : Win32_COLORREF    := RGB(Colour);
  begin
    if Width > 0 then
      Handle_Type(P.all).Handle := Handle (CreatePen(0,Win32_INT(Width),S));
    else
      Handle_Type(P.all).Handle := Handle (GetStockObject(NULL_PEN));
    end if;
    Add (Canvas, "Set_Pen", P);
  end Set_Pen;

  ----------------------------------------------------------------------------
  --
  --  Set_Fill: add a solid brush handle to the drawing list.
  --
  procedure Set_Fill (Canvas : in Canvas_Type;
                      Colour : in Colour_Type) is
    P : Canvas_Object_Ptr := new Handle_Type;
    S : Win32_COLORREF    := RGB(Colour);
  begin
    Handle_Type(P.all).Handle := Handle (CreateSolidBrush(S));
    Add (Canvas, "Set_Fill", P);
  end Set_Fill;

  ----------------------------------------------------------------------------
  --
  --  Set_Fill: add a transparent brush handle to the drawing list.
  --
  procedure Set_Fill (Canvas : in Canvas_Type) is
    P : Canvas_Object_Ptr := new Handle_Type;
    L : aliased Win32_LOGBRUSH;
  begin
    L.lbStyle := BS_HOLLOW;
    Handle_Type(P.all).Handle :=
                        Handle (CreateBrushIndirect(L'Unchecked_Access));
    Add (Canvas, "Set_Fill", P);
  end Set_Fill;

  ----------------------------------------------------------------------------
  --
  --  Draw_Text: add a text string to the drawing with the top left
  --             corner at the specified point.
  --
  procedure Draw_Text (Canvas   : in Canvas_Type;
                       From     : in Point_Type;
                       Text     : in String) is
    P : Canvas_Object_Ptr  := new Text_Type (Text'Length);
  begin
    Text_Type(P.all).Text  := Text;
    Text_Type(P.all).From  := From;
    Text_Type(P.all).To    := From;
    Text_Type(P.all).Align := -1;
    Add (Canvas, "Draw_Text", P);
  end Draw_Text;

  ----------------------------------------------------------------------------
  --
  --  Draw_Text: add a text string to the drawing within a rectangle
  --             specified by diagonally opposite corners.
  --
  procedure Draw_Text (Canvas   : in Canvas_Type;
                       From     : in Point_Type;
                       To       : in Point_Type;
                       Text     : in String;
                       Align    : in Alignment_Type := Left) is
    P : Canvas_Object_Ptr  := new Text_Type (Text'Length);
  begin
    Text_Type(P.all).Text  := Text;
    Text_Type(P.all).From  := From;
    Text_Type(P.all).To    := To;
    Text_Type(P.all).Align := Alignment_Type'Pos(Align);
    Add (Canvas, "Draw_Text", P);
  end Draw_Text;

  ----------------------------------------------------------------------------
  --
  --  Draw_Text: calculate the text rectangle from a height and width.
  --
  procedure Draw_Text (Canvas   : in Canvas_Type;
                       From     : in Point_Type;
                       Width    : in Integer;
                       Height   : in Integer;
                       Text     : in String;
                       Align    : in Alignment_Type := Left) is
  begin
    Draw_Text (Canvas, From, (From.X+Width,From.Y+Height), Text, Align);
  end Draw_Text;

  ----------------------------------------------------------------------------
  --
  --  Draw_Line: add a line to the drawing between two points.
  --
  procedure Draw_Line (Canvas : in Canvas_Type;
                       From   : in Point_Type;
                       To     : in Point_Type) is
    P : Canvas_Object_Ptr := new Line_Type;
  begin
    Line_Type(P.all).From := From;
    Line_Type(P.all).To   := To;
    Add (Canvas, "Draw_Line", P);
  end Draw_Line;

  ----------------------------------------------------------------------------
  --
  --  Draw_Line: calculate the line endpoint from a length and angle.
  --
  procedure Draw_Line (Canvas : in Canvas_Type;
                       From   : in Point_Type;
                       Length : in Positive;
                       Angle  : in Angle_Type) is
    To : Point_Type;
  begin
    To := Endpoint(From,Length,Angle);
    Draw_Line (Canvas, From, To);
  end Draw_Line;

  ----------------------------------------------------------------------------
  --
  --  Draw_Line_List: add a polyline to the drawing. Ignore polylines with
  --                  less than two points, and draw an ordinary line for a
  --                  polyline with only two points.
  --
  procedure Draw_Line_List (Canvas : in Canvas_Type;
                            Points : in Point_List) is
    P : Window_Ptr := Get_Internals (Canvas, "Draw_Line_List");
  begin
    if Points'Length = 2 then
      Draw_Line (Canvas, Points(Points'First), Points(Points'Last));
    elsif Points'Length > 2 then
      declare
        P : Canvas_Object_Ptr := new Polyline_Type(Points'Length);
      begin
        P.Next := null;
        for I in 1..Points'Length loop
          Polyline_Type(P.all).Points(I) :=
                            (Win32_LONG(Points(Points'First+I-1).X),
                             Win32_LONG(Points(Points'First+I-1).Y));
        end loop;
        Add (Canvas, "Draw_Line_List", P);
      end;
    end if;
  end Draw_Line_List;

  ----------------------------------------------------------------------------
  --
  --  Draw_Rectangle: add either a normal rectangle or a rounded rectangle
  --                  to the drawing, depending on whether the rounding is
  --                  zero or not.
  --
  procedure Draw_Rectangle (Canvas   : in Canvas_Type;
                            From     : in Point_Type;
                            To       : in Point_Type;
                            Rounding : in Point_Type := (0,0)) is
    P : Canvas_Object_Ptr;
  begin
    if Rounding = (0,0) then
      P := new Rectangle_Type;
      Rectangle_Type(P.all).From := From;
      Rectangle_Type(P.all).To   := To;
    else
      P := new Rounded_Rectangle_Type;
      Rounded_Rectangle_Type(P.all).From   := From;
      Rounded_Rectangle_Type(P.all).To     := To;
      Rounded_Rectangle_Type(P.all).Corner := Rounding;
    end if;
    Add (Canvas, "Draw_Rectangle", P);
  end Draw_Rectangle;

  ----------------------------------------------------------------------------
  --
  --  Draw_Rectangle: calculate the rectangle size from a height and width.
  --
  procedure Draw_Rectangle (Canvas   : in Canvas_Type;
                            From     : in Point_Type;
                            Width    : in Positive;
                            Height   : in Positive;
                            Rounding : in Point_Type := (0,0)) is
  begin
    Draw_Rectangle (Canvas, From, (From.X+Width, From.Y+Height), Rounding);
  end Draw_Rectangle;

  ----------------------------------------------------------------------------
  --
  --  Draw_Ellipse: draw an ellipse whose size is specified by a bounding
  --                rectangle.
  --
  procedure Draw_Ellipse (Canvas : in Canvas_Type;
                          From   : in Point_Type;
                          To     : in Point_Type) is
    P : Canvas_Object_Ptr := new Ellipse_Type;
  begin
    Ellipse_Type(P.all).From := From;
    Ellipse_Type(P.all).To   := To;
    Add (Canvas, "Draw_Ellipse", P);
  end Draw_Ellipse;

  ----------------------------------------------------------------------------
  --
  --  Draw_Ellipse: calculate the bounding rectangle from a height and width.
  --
  procedure Draw_Ellipse (Canvas : in Canvas_Type;
                          From   : in Point_Type;
                          Width  : in Positive;
                          Height : in Positive) is
  begin
    Draw_Ellipse (Canvas, From, (From.X + Width, From.Y + Height));
  end Draw_Ellipse;

  ----------------------------------------------------------------------------
  --
  --  Draw_Circle: draw an ellipse in a bounding square calculated from
  --               the centre point and the radius.
  --
  procedure Draw_Circle (Canvas : in Canvas_Type;
                         Centre : in Point_Type;
                         Radius : in Positive) is
    P : Window_Ptr := Get_Internals (Canvas, "Draw_Circle");
  begin
    Draw_Ellipse (Canvas, (Centre.X - Radius,Centre.Y - Radius),
                          (Centre.X + Radius,Centre.Y + Radius));
  end Draw_Circle;

  ----------------------------------------------------------------------------
  --
  --  Draw_Polygon: create and fill a Windows-style array with the coordinates
  --                of the vertices, then add the polygon to the drawing list.
  --                Draw a line if there are only two vertices, and do nothing
  --                if there are less than two vertices.
  --
  procedure Draw_Polygon (Canvas : in Canvas_Type;
                          Points : in Point_List) is
    P : Window_Ptr := Get_Internals (Canvas, "Draw_Polygon");
  begin
    if Points'Length = 2 then
      Draw_Line (Canvas, Points(Points'First), Points(Points'Last));
    elsif Points'Length > 2 then
      declare
        P : Canvas_Object_Ptr := new Polygon_Type(Points'Length);
      begin
        P.Next := null;
        for I in 1..Points'Length loop
          Polygon_Type(P.all).Points(I) :=
                            (Win32_LONG(Points(Points'First+I-1).X),
                             Win32_LONG(Points(Points'First+I-1).Y));
        end loop;
        Add (Canvas, "Draw_Polygon", P);
      end;
    end if;
  end Draw_Polygon;

  ----------------------------------------------------------------------------
  --
  --  Draw_Image: draw the specified image on the canvas starting at the
  --              specified top-left corner point.
  --
  procedure Draw_Image (Canvas : in Canvas_Type;
                        From   : in Point_Type;
                        Image  : in Image_Type) is
    I : Image_Ptr;
  begin
    if Valid(Image) then
      I := Image_Ptr(Image.Internals.Pointer);
      Draw_Image (Canvas, From, I.Width, I.Height, Image);
    end if;
  end Draw_Image;

  ----------------------------------------------------------------------------
  --
  --  Draw_Image: draw the specified image on the canvas starting at the
  --              specified top-left corner point, stretching it to the
  --              specified bottom-right corner point.
  --
  procedure Draw_Image (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        To       : in Point_Type;
                        Image    : in Image_Type) is
    L : Integer := Integer'Min(From.X,To.X);
    T : Integer := Integer'Min(From.Y,To.Y);
    R : Integer := Integer'Max(From.X,To.X);
    B : Integer := Integer'Max(From.Y,To.Y);
  begin
    Draw_Image (Canvas, (L,T), R-L, B-T, Image);
  end Draw_Image;

  ----------------------------------------------------------------------------
  --
  --  Draw_Image: draw the specified image on the canvas starting at the
  --              specified top-left corner point, stretching it to the
  --              specified width and height.
  --
  procedure Draw_Image (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Width    : in Natural;
                        Height   : in Natural;
                        Image    : in Image_Type) is
    P : Window_Ptr := Get_Internals (Canvas, "Draw_Image");
    B : Canvas_Object_Ptr := new Bitmap_Type;
    I : Image_Ptr;
  begin
    if Valid(Image) then
      I := Image_Ptr(Image.Internals.Pointer);
      Bitmap_Type(B.all).From   := From;
      Bitmap_Type(B.all).Width  := Width;
      Bitmap_Type(B.all).Height := Height;
      Bitmap_Type(B.all).Bitmap := Image.Internals;
      Add (Canvas, "Draw_Image", B);
    end if;
  end Draw_Image;

  ----------------------------------------------------------------------------
  --
  --  Mouse_Down: test whether the mouse was pressed within a specific
  --              canvas.
  --
  function Mouse_Down (Canvas : Canvas_Type) return Boolean is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "Mouse_Down"));
  begin
    return C.Monitor.Mouse_Down;
  end Mouse_Down;

  ----------------------------------------------------------------------------
  --
  --  Mouse_Moved: test if the mouse has moved within a canvas, which
  --               will only be true while the mouse is down after being
  --               pressed inside this canvas.
  --
  function Mouse_Moved (Canvas : Canvas_Type) return Boolean is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "Mouse_Moved"));
  begin
    return C.Monitor.Mouse_Moved;
  end Mouse_Moved;

  ----------------------------------------------------------------------------
  --
  --  Start_Point: get the position where the mouse was pressed within the
  --               specified canvas.
  --
  function Start_Point (Canvas : Canvas_Type) return Point_Type is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "Start_Point"));
    P : Point_Type;
  begin
    C.Monitor.Get_Start (P.X, P.Y);
    return P;
  end Start_Point;

  ----------------------------------------------------------------------------
  --
  --  End_Point: get the latest mouse position within the specified canvas.
  --
  function End_Point (Canvas : Canvas_Type) return Point_Type is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "End_Point"));
    P : Point_Type;
  begin
    C.Monitor.Get_End (P.X, P.Y);
    return P;
  end End_Point;

  ----------------------------------------------------------------------------
  --
  --  Key_Code: get the latest key position within the specified canvas.
  --
  function Key_Code (Canvas : Canvas_Type) return Character is
    C : Canvas_Ptr := Canvas_Ptr(Get_Internals(Canvas, "Key_Code"));
    K : Character;
  begin
    C.Monitor.Get_Key (K);
    return K;
  end Key_Code;

  ----------------------------------------------------------------------------
  --
  --           C O M M O N   D I A L O G   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Execute: execute a common dialog by asking the message loop to call
  --           its Show_Dialog primitive and return the result.
  --
  function Execute (Dialog : Common_Dialog_Type) return Boolean is
    P : Common_Dialog_Ptr := Get_Internals (Dialog, "Execute");
    B : Boolean;
  begin
    Message_Loop.Show_Dialog (P,B);
    return B;
  end Execute;

  ----------------------------------------------------------------------------
  --
  --  Colour_Dialog: construct a colour dialog.
  --
  function Colour_Dialog return Colour_Dialog_Type is
    D : Colour_Dialog_Type;
    P : Colour_Dialog_Ptr := new Colour_Dialog_Internals;
  begin
    D.Internals.Pointer := Reference_Counted_Ptr(P);
    P.Struct.lStructSize  := P.Struct'Size / Win32_BYTE'Size;
    P.Struct.lpCustColors := P.Custom(P.Custom'First)'Access;
    return D;
  end Colour_Dialog;

  ----------------------------------------------------------------------------
  --
  --  Set_Colour: set the colour stored in a colour dialog.
  --
  procedure Set_Colour (Dialog : in Colour_Dialog_Type;
                        Colour : in Colour_Type) is
    P : Colour_Dialog_Ptr := Colour_Dialog_Ptr(Get_Internals(Dialog,
                                                             "Set_Colour"));
  begin
    P.Colour := Colour;
  end Set_Colour;

  ----------------------------------------------------------------------------
  --
  --  Get_Colour: get the colour stored in a colour dialog.
  --
  function Get_Colour (Dialog : in Colour_Dialog_Type) return Colour_Type is
    P : Colour_Dialog_Ptr := Colour_Dialog_Ptr(Get_Internals(Dialog,
                                                             "Get_Colour"));
  begin
    return P.Colour;
  end Get_Colour;

  ----------------------------------------------------------------------------
  --
  --  Font_Dialog: construct a font dialog.
  --
  function Font_Dialog return Font_Dialog_Type is
    D : Font_Dialog_Type;
    P : Font_Dialog_Ptr := new Font_Dialog_Internals;
  begin
    D.Internals.Pointer := Reference_Counted_Ptr(P);
    P.Struct.lStructSize := P.Struct'Size / Win32_BYTE'Size;
    P.Struct.lpLogFont   := P.Font'Access;
    P.Font  := Set_Font(Font("Arial",9));
    return D;
  end Font_Dialog;

  ----------------------------------------------------------------------------
  --
  --  Set_Font: set the font stored in a font dialog.
  --
  procedure Set_Font (Dialog : in Font_Dialog_Type;
                      Font   : in Font_Type) is
    P : Font_Dialog_Ptr := Font_Dialog_Ptr(Get_Internals(Dialog,
                                                         "Set_Font"));
  begin
    P.Font := Set_Font(Font);
  end Set_Font;

  ----------------------------------------------------------------------------
  --
  --  Get_Font: get the font stored in a font dialog.
  --
  function Get_Font (Dialog : in Font_Dialog_Type) return Font_Type is
    P : Font_Dialog_Ptr := Font_Dialog_Ptr(Get_Internals(Dialog,
                                                         "Get_Font"));
  begin
    return Get_Font (P.Font);
  end Get_Font;

  ----------------------------------------------------------------------------
  --
  --  Set_Name: set the filename stored in a file dialog.
  --
  procedure Set_Name (Dialog : in File_Dialog_Type;
                      Name   : in String) is
    P : File_Dialog_Ptr := File_Dialog_Ptr(Get_Internals(Dialog,
                                                         "Set_Name"));
  begin
    if P.Buffer'Length > Name'Length then
      P.Buffer(P.Buffer'First..P.Buffer'First+Name'Length) := To_Array(Name);
    else
      P.Buffer := To_Array(Name(Name'First..Name'First+P.Buffer'Length-2));
    end if;
  end Set_Name;

  ----------------------------------------------------------------------------
  --
  --  Get_Name: get the filename stored in a file dialog.
  --
  function Get_Name (Dialog : in File_Dialog_Type) return String is
    P : File_Dialog_Ptr := File_Dialog_Ptr(Get_Internals(Dialog,
                                                         "Get_Name"));
  begin
    return To_String(P.Buffer);
  end Get_Name;

  ----------------------------------------------------------------------------
  --
  --  Add_Filter: add a filename filter to a file dialog.
  --
  procedure Add_Filter (Dialog : in File_Dialog_Type;
                        Text   : in String;
                        Filter : in String) is
    P : File_Dialog_Ptr := File_Dialog_Ptr(Get_Internals(Dialog,
                                                         "Add_Filter"));
  begin
    if P.Length + Text'Length + Filter'Length + 2 < P.Filter'Length then
      P.Filter (P.Filter'First+P.Length ..
                P.Filter'First+P.Length+Text'Length) := To_Array(Text);
      P.Length := P.Length + Text'Length + 1;
      P.Filter (P.Filter'First+P.Length ..
                P.Filter'First+P.Length+Filter'Length) := To_Array(Filter);
      P.Length := P.Length + Filter'Length + 1;
      P.Filter (P.Filter'First+P.Length) := Win32_CHAR'First;
      if P.Struct.lpstrFilter = null then
        P.Struct.lpstrFilter := To_LPCSTR(P.Filter);
      end if;
    end if;
  end Add_Filter;

  ----------------------------------------------------------------------------
  --
  --  Set_Directory: select the initial directory for a file dialog.
  --
  procedure Set_Directory (Dialog : in File_Dialog_Type;
                           Name   : in String) is
    P : File_Dialog_Ptr := File_Dialog_Ptr(Get_Internals(Dialog,
                                                         "Set_Directory"));
    L : Win32_SIZE := Win32_SIZE'Min(Name'Length,P.Directory'Length-1);
  begin
    P.Directory (P.Directory'First .. P.Directory'First+L) :=
                        To_Array(Name(Name'First..Name'First+Integer(L)-1));
    if P.Struct.lpstrInitialDir = null then
      P.Struct.lpstrInitialDir := To_LPCSTR(P.Directory);
    end if;
  end Set_Directory;

  ----------------------------------------------------------------------------
  --
  --  Open_Dialog: construct a file open dialog.
  --
  function Open_Dialog (Title : String) return Open_Dialog_Type is
    D : Open_Dialog_Type;
    P : File_Dialog_Ptr := new Open_Dialog_Internals (Title'Length+1);
  begin
    D.Internals.Pointer  := Reference_Counted_Ptr(P);
    P.Struct.lStructSize := P.Struct'Size / Win32_BYTE'Size;
    P.Struct.lpstrTitle  := To_LPCSTR(P.Title);
    P.Struct.lpstrFile   := P.Buffer(P.Buffer'First)'Access;
    P.Struct.nMaxFile    := P.Buffer'Length;
    P.Struct.Flags       := OFN_HIDEREADONLY or OFN_FILEMUSTEXIST or
                            OFN_PATHMUSTEXIST;
    P.Buffer(P.Buffer'First) := Win32_CHAR'Val(0);
    P.Title := To_Array(Title);
    return D;
  end Open_Dialog;

  ----------------------------------------------------------------------------
  --
  --  Save_Dialog: construct a file save dialog.
  --
  function Save_Dialog (Title  : String;
                        Create : Boolean := True) return Save_Dialog_Type is
    D : Save_Dialog_Type;
    P : File_Dialog_Ptr := new Save_Dialog_Internals (Title'Length+1);
  begin
    D.Internals.Pointer  := Reference_Counted_Ptr(P);
    P.Struct.lStructSize := P.Struct'Size / Win32_BYTE'Size;
    P.Struct.lpstrTitle  := To_LPCSTR(P.Title);
    P.Struct.lpstrFile   := P.Buffer(P.Buffer'First)'Access;
    P.Struct.nMaxFile    := P.Buffer'Length;
    P.Struct.Flags       := OFN_HIDEREADONLY or OFN_PATHMUSTEXIST;
    if Create then
      P.Struct.Flags := P.Struct.Flags or OFN_OVERWRITEPROMPT;
    else
      P.Struct.Flags := P.Struct.Flags or OFN_CREATEPROMPT;
    end if;
    P.Buffer(1) := Win32_CHAR'Val(0);
    P.Title := To_Array(Title);
    return D;
  end Save_Dialog;

------------------------------------------------------------------------------
--
--               P A C K A G E   I N I T I A L I S A T I O N
--
--    Register the window classes if there is no previous module instance.
--
------------------------------------------------------------------------------

begin
  if Get_hPrevInstance = System.Null_Address then
    declare
      Class : aliased Win32_WNDCLASS;
      Dummy : Win32_ATOM;
    begin

      -- Set up general window class information

      Class.Style         := CS_HREDRAW or CS_VREDRAW;
      Class.cbClsExtra    := 0;
      Class.cbWndExtra    := 0;
      Class.hInstance     := Get_hInstance;
      Class.hIcon         := LoadIcon(System.Null_Address,
                                      To_LPCSTR(IDI_APPLICATION));
      Class.hCursor       := LoadCursor(System.Null_Address,
                                        To_LPCSTR(IDC_ARROW));
      Class.hbrBackground := To_Handle(COLOR_BTNFACE+1);
      Class.lpszMenuName  := null;

      -- Set frame-specific information and register the frame class

      Class.lpszClassName := To_LPCSTR(Frame_Class);
      Class.lpfnWndProc   := Frame_Proc'Access;

      Dummy := RegisterClass(Class'Unchecked_Access);

      -- Set dialog-specific information and register the dialog class

      Class.lpszClassName := To_LPCSTR(Dialog_Class);
      Class.lpfnWndProc   := Dialog_Proc'Access;

      Dummy := RegisterClass(Class'Unchecked_Access);

      -- Set canvas-specific information and register the canvas class

      Class.lpszClassName := To_LPCSTR(Canvas_Class);
      Class.hbrBackground := System.Null_Address;
      Class.lpfnWndProc   := Canvas_Proc'Access;

      Dummy := RegisterClass(Class'Unchecked_Access);
    end;
  end if;
end JEWL.Windows;
------------------------------------------------------------------------------
--                                                                          --
--                         J E W L . W I N D O W S                          --
--                                                                          --
--   A package for developing GUI-based programs for beginners.             --
--                                                                          --
--   This is a large package, but splitting it into child packages would    --
--   require multiple generic instantiations in order to use it.            --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-windows.ads 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-windows.ads $
-- Revision 1.7  2007/01/08 17:00:00  JE
-- * Fixed linker options in JEWL.Win32_Interface to accommodate changes to GNAT
--   GPL 2006 compiler (thanks to John McCormick for this)
-- * Added delay in message loop to avoid the appearance of hogging 100% of CPU
--   time
--
-- Revision 1.6  2001/11/02 16:00:00  JE
-- * Fixed canvas bug when saving an empty canvas
-- * Restore with no prior save now acts as erase
-- * Removed redundant variable declaration in Image function
--
-- Revision 1.5  2001/08/22 15:00:00  JE
-- * Minor bugfix to Get_Text for combo boxes
-- * Minor changes to documentation (including new example involving dialogs)
--
-- Revision 1.4  2001/01/25 09:00:00  je
-- Changes visible to the user:
--
-- * Added support for drawing bitmaps on canvases (Draw_Image operations
--   and new type Image_Type)
-- * Added Play_Sound
-- * Added several new operations on all windows: Get_Origin, Get_Width,
--   Get_Height, Set_Origin, Set_Size and Focus
-- * Added several functions giving screen and window dimensions: Screen_Width,
--   Screen_Height, Frame_Width, Frame_Height, Dialog_Width, Dialog_Height and
--   Menu_Height
-- * Canvases can now handle keyboard events: new constructor and Key_Code added
-- * Added procedure Play_Sound
-- * Operations "+" and "-" added for Point_Type
-- * Pens can now be zero pixels wide
-- * The absolute origin of a frame can now have be specified when the frame
--   is created
-- * Added new File_Dialog operations Add_Filter and Set_Directory
-- * Added Get_Line renames to JEWL.IO for compatibility with Ada.Text_IO
-- * Added all the Get(File,Item) operations mentioned in documentation but
--   unaccountably missing :-(
-- * Documentation updated to reflect the above changes
-- * HTML versions of public package specifications added with links from
--   main documentation pages
--
-- Other internal changes:
--
-- * Canvas fonts, pens etc. now use JEWL.Reference_Counted_Type rather than
--   reinventing this particular wheel, as do images
-- * Various minor code formatting changes: some code reordered for clarity,
--   some comments added or amended,
-- * Changes introduced in 1.2 to support GNAT 3.10 have been reversed, since
--   GNAT 3.10 still couldn't compile this code correctly... ;-(
--
-- Outstanding issues:
--
-- * Optimisation breaks the code (workaround: don't optimise)
--
-- Revision 1.3  2000/07/07 12:00:00  je
-- * JEWL.Simple_Windows added; JEWL.IO modified to use JEWL.Simple_Windows.
-- * JEWL.IO bug fix: Put_Line to file wrote newline to standard output
--   instead of to the file (thanks to Jeff Carter for pointing this out).
-- * Panels fixed so that mouse clicks are passed on correctly to subwindows.
-- * Memos fixed so that tabs are handled properly.
-- * Password feature added to editboxes.
-- * Minor typos fixed in comments within the package sources.
-- * Documentation corrected and updated following comments from Moti Ben-Ari
--   and Don Overheu.
--
-- Revision 1.2  2000/04/18 20:00:00  je
-- * Minor code changes to enable compilation by GNAT 3.10
-- * Minor documentation errors corrected
-- * Some redundant "with" clauses removed
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

generic                         -- This is a generic package which must be
  type Command_Type is (<>);    -- instantiated with a list of possible
                                -- command values to be generated whenever
package JEWL.Windows is         -- a window is closed, a button is clicked
                                -- or a menu item is selected.

  ----------------------------------------------------------------------------
  --  Miscellaneous operations
  ----------------------------------------------------------------------------

  procedure Show_Error   (Text  : in String;   -- an error message
                          Title : in String := "Error");
  function  Show_Query   (Text  : in String;   -- a yes/no query
                          Title : in String := "Query")
                                return Boolean;
  procedure Show_Message (Text  : in String;   -- an information message
                          Title : in String := "Message");

  procedure Play_Sound   (Sound : in String);  -- play a sound file

  function  Screen_Width  return Natural;      -- width of display screen
  function  Screen_Height return Natural;      -- height of display screen

  ----------------------------------------------------------------------------
  --
  --                      S U P P O R T   T Y P E S
  --
  --  Except for Alignment_Type, these types are defined in the top-level
  --  package JEWL and are renamed here for convenience:
  --
  --  Alignment_Type  : used to specify text alignment with a window.
  --  Angle_Type      : an angle specified as an integral number of
  --                    degrees (0 to 359)
  --  Colour_Type     : a colour specified as an RGB value.
  --  Font_Type       : a font specified by a name, point size and bold
  --                    and italic style options.
  --  Point_Type      : a pair of (X,Y) coordinates within a window.
  --  Point_List      : an array of (X,Y) coordinate pairs.
  --
  ----------------------------------------------------------------------------

  type    Alignment_Type is (Left, Centre, Right);
  subtype Angle_Type     is JEWL.Angle_Type;
  subtype Colour_Range   is JEWL.Colour_Range;
  subtype Colour_Type    is JEWL.Colour_Type;
  subtype Font_Type      is JEWL.Font_Type;
  type    Image_Type     is private;
  subtype Point_Type     is JEWL.Point_Type;
  subtype Point_List     is JEWL.Point_List;

  ----------------------------------------------------------------------------
  --  Renamings for our transatlantic cousins, in the hope that some day
  --  they'll repay the favour/favor... ;-)
  ----------------------------------------------------------------------------

  subtype  Color_Range is Colour_Range;
  subtype  Color_Type  is Colour_Type;
  function Center      return Alignment_Type renames Centre;

  ----------------------------------------------------------------------------
  --
  --        O P E R A T I O N S   O N   S U P P O R T   T Y P E S
  --
  --  These are renamings of the operations defined in the parent package
  --  provided for convenience.
  --
  --  Colour operations:
  --    Light    : Generate a lightened version of a colour, e.g. Light(Red).
  --    Dark     : Generate a darkened version of a colour, e.g. Dark(Green).
  --
  --  Font operations:
  --    Font     : Generate a font with the specified properties.
  --    Name     : Get the name of the font typeface.
  --    Size     : Get the font size in points.
  --    Bold     : Test if the font is bold.
  --    Italic   : Test if the font is italic.
  --
  --  Image operations:
  --    Image    : Load an image from a bitmap file.
  --    Valid    : Test if an image is valid.
  --    Width    : Get the width of an image.
  --    Height   : Get the height of an image.
  --
  --  Point operations:
  --    Endpoint : Calculate the endpoint of a line from a starting point,
  --               length and angle
  --    Inside   : Test if a specified point is inside a specified rectangle
  --               (defined by the coordinates of two diagonally opposite
  --               corners).
  --    P1 + P2  : Add two points (P1.X+P2.X, P1.Y+P2.Y).
  --    P1 - P2  : Subtract two points (P1.X-P2.X, P1.Y-P2.Y).
  --
  ----------------------------------------------------------------------------

  function Light    (Colour : JEWL.Colour_Type) return  JEWL.Colour_Type
                                                renames JEWL.Light;
  function Dark     (Colour : JEWL.Colour_Type) return  JEWL.Colour_Type
                                                renames JEWL.Dark;

  function Font     (Name   : String;
                     Size   : Positive;
                     Bold   : Boolean := False;
                     Italic : Boolean := False) return  JEWL.Font_Type
                                                renames JEWL.Font;
  function Name     (Font   : Font_Type)        return String
                                                renames JEWL.Name;
  function Size     (Font   : Font_Type)        return Natural
                                                renames JEWL.Size;
  function Bold     (Font   : Font_Type)        return Boolean
                                                renames JEWL.Bold;
  function Italic   (Font   : Font_Type)        return Boolean
                                                renames JEWL.Italic;

  function Image    (Name   : String)           return Image_Type;
  function Valid    (Image  : Image_Type)       return Boolean;
  function Width    (Image  : Image_Type)       return Natural;
  function Height   (Image  : Image_Type)       return Natural;

  function Endpoint (From   : JEWL.Point_Type;
                     Length : Positive;
                     Angle  : JEWL.Angle_Type)  return  JEWL.Point_Type
                                                renames JEWL.Endpoint;
  function Inside   (Point  : JEWL.Point_Type;
                     From   : JEWL.Point_Type;
                     To     : JEWL.Point_Type)  return  Boolean
                                                renames JEWL.Inside;
  function "+"      (P1, P2 : Point_Type)       return Point_Type
                                                renames JEWL."+";
  function "-"      (P1, P2 : Point_Type)       return Point_Type
                                                renames JEWL."-";

  ----------------------------------------------------------------------------
  --
  --             S U P P O R T   T Y P E   C O N S T A N T S
  --
  --  Angles  : North, South, East and West
  --  Colours : Black, White, Red, Green, Blue, etc.
  --  Fonts   : Default_Font (the default font for top-level windows) and
  --            Parent_Font (the same font as a window's parent uses)
  --
  ----------------------------------------------------------------------------

  North        : constant Angle_Type  :=   0;
  South        : constant Angle_Type  := 180;
  East         : constant Angle_Type  :=  90;
  West         : constant Angle_Type  := 270;

  Black        : constant Colour_Type := (  0,  0,  0);
  White        : constant Colour_Type := (255,255,255);
  Red          : constant Colour_Type := (255,  0,  0);
  Green        : constant Colour_Type := (  0,255,  0);
  Blue         : constant Colour_Type := (  0,  0,255);
  Gray         : constant Colour_Type := (128,128,128);
  Yellow       : constant Colour_Type := (255,255,  0);
  Cyan         : constant Colour_Type := (  0,255,255);
  Magenta      : constant Colour_Type := (255,  0,255);

  Default_Font : constant Font_Type   := Font("Arial",9);
  Parent_Font  : constant Font_Type   := Font("",1);

  ----------------------------------------------------------------------------
  --
  --                        W I N D O W _ T Y P E
  --
  --  An abstract class providing basic behaviour which all windows share.
  --
  ----------------------------------------------------------------------------

  type Window_Type is abstract tagged private;

  Invalid_Window : exception;   -- raised if an attempt is made to use an
                                -- invalid (non-open) window

  ----------------------------------------------------------------------------
  --
  --  Window operations (common to all windows):
  --
  --  Show       (Window,   -- make the window visible or invisible depending
  --              Visible)  -- on the value of Visible (default: True).
  --  Hide       (Window)   -- make the window invisible.
  --  Focus      (Window)   -- give the window the input focus.
  --  Visible    (Window)   -- return True if the window is visible.
  --  Get_Origin (Window)   -- get the origin (top left point) of the
  --                        -- specified window.
  --  Get_Width  (Window)   -- get the width of the specified window.
  --  Get_Height (Window)   -- get the height of the specified window.
  --  Set_Origin (Window,   -- set the origin (top left point) of the
  --              Origin)   -- specified window to this value.
  --  Set_Size   (Window,   -- set the size of the specified window
  --              Width,    -- to this width (optional)
  --              Height)   -- and this height (optional).
  --  Get_Font   (Window)   -- get the font for the specified window.
  --  Set_Font   (Window,   -- set the font for the specified window
  --              Font)     -- to this one.
  --
  ----------------------------------------------------------------------------

  procedure Show       (Window  : in Window_Type;
                        Visible : in Boolean := True);
  procedure Hide       (Window  : in Window_Type);
  procedure Focus      (Window  : in Window_Type);
  function  Visible    (Window  : Window_Type) return Boolean;

  function  Get_Origin (Window  : Window_Type) return Point_Type;
  function  Get_Width  (Window  : Window_Type) return Natural;
  function  Get_Height (Window  : Window_Type) return Natural;

  procedure Set_Origin (Window  : in Window_Type;
                        Origin  : in Point_Type);
  procedure Set_Size   (Window  : in Window_Type;
                        Width   : in Natural := 0;
                        Height  : in Natural := 0);

  function  Get_Font   (Window  : Window_Type) return Font_Type;
  procedure Set_Font   (Window  : in Window_Type;
                        Font    : in Font_Type);

  ----------------------------------------------------------------------------
  --
  --                  W I N D O W   S U B C L A S S E S
  --
  --  The primary window subclasses are containers and controls.  They
  --  share the behaviour common to all windows (above) and provide extra
  --  behaviour as well.
  --
  --  Container_Type : the abstract base type for all containers.
  --  Control_Type   : the abstract base type for all controls.
  --
  ----------------------------------------------------------------------------

  type Container_Type is abstract new Window_Type with private;
  type Control_Type   is abstract new Window_Type with private;

  ----------------------------------------------------------------------------
  --
  --                         C O N T A I N E R S
  --
  --  Containers are windows which can contain other windows. All windows
  --  except frames and dialogs (see below) must be contained within some
  --  other container window. There are some restrictions on the types of
  --  container that a window can be attached to (for example, a menu item
  --  must be attached to a menu).
  --
  --  Most windows specify an origin, a width and a height whose coordinates
  --  are taken relative to the enclosing container. Positive widths and
  --  heights are absolute values, but zero and negative widths and heights
  --  are interpreted as being relative to the width and height of the
  --  enclosing container (so a width of 0 means the width of the enclosing
  --  container, a height of -10 means 10 pixels less than the height of the
  --  enclosing container).
  --
  --  The types of container windows available are as follows:
  --
  --  Frame_Type  : a main window with a title bar, system menu, minimise
  --                and maximise buttons, and a close button.
  --  Dialog_Type : a top level window which is used for modal interaction,
  --                disabling other windows while the interaction takes
  --                place.
  --  Panel_Type  : a plain window which is used as a container for other
  --                subwindows.
  --  Menu_Type   : a menu which can contain menu items and submenus.
  --
  ----------------------------------------------------------------------------

  type Frame_Type  is new Container_Type with private;
  type Dialog_Type is new Container_Type with private;
  type Panel_Type  is new Container_Type with private;
  type Menu_Type   is new Container_Type with private;

  ----------------------------------------------------------------------------
  --
  --                             F R A M E S
  --
  --  A frame is a top level window with a title bar, system menu, minimise
  --  and maximise buttons, and a close button. Closing a frame generates a
  --  command. Frames are normally visible, but can be hidden if required.
  --  A frame should be used as the main window for an application.
  --
  --  Frame operations:
  --
  --  Frame   (Origin,      -- create a frame at the specified position
  --           Width,       -- with the specified width
  --           Height,      -- and height in pixels,
  --           Title,       -- with the specified title in the title bar,
  --           Command,     -- generating this command when it is closed,
  --           Font)        -- using this font (default: Default_Font).
  --  Frame   (Width,       -- create a frame with the specified width
  --           Height,      -- and height in pixels, placed randomly,
  --           Title,       -- with the specified title in the title bar,
  --           Command,     -- generating this command when it is closed,
  --           Font)        -- using this font (default: Default_Font).
  --
  --  Close   (Frame)       -- close the frame.
  --  Valid   (Frame)       -- true until the frame is closed.
  --
  --  Frame_Width           -- return the width of the frame border.
  --  Frame_Height          -- return the height of the frame border.
  --
  --  Next_Command          -- return the next command generated by any
  --                        -- control in any existing frame.
  --  Command_Ready         -- test if there is a command pending
  --
  ----------------------------------------------------------------------------

  function  Frame   (Origin  : Point_Type;
                     Width   : Positive;
                     Height  : Positive;
                     Title   : String;
                     Command : Command_Type;
                     Font    : Font_Type := Default_Font) return Frame_Type;
  function  Frame   (Width   : Positive;
                     Height  : Positive;
                     Title   : String;
                     Command : Command_Type;
                     Font    : Font_Type := Default_Font) return Frame_Type;

  procedure Close   (Frame   : in Frame_Type);
  function  Valid   (Frame   : Frame_Type) return Boolean;

  function  Frame_Width   return Natural;
  function  Frame_Height  return Natural;

  function  Next_Command  return Command_Type;
  function  Command_Ready return Boolean;

  ----------------------------------------------------------------------------
  --
  --                            D I A L O G S
  --
  --  A dialog is a top level window like a frame, but it only has a close
  --  button on its title bar. Dialogs are intended for user interaction.
  --  When a dialog is executed it becomes visible and all other windows
  --  are disabled. Execution of a dialog continues until a command is
  --  generated by closing the dialog window or by clicking on a button
  --  attached to the dialog. Dialog windows do not have a menu bar.
  --
  --  Dialog operations:
  --
  --  Dialog  (Width,       -- create a dialog with the given width and
  --           Height,      -- and height in pixels,
  --           Title,       -- with the specified title in the title bar,
  --           Command,     -- generating this command when it is closed,
  --           Font)        -- using this font (default: Default_Font).
  --
  --  Execute (Dialog)      -- execute a dialog and return the command code
  --                        -- used to exit from it.
  --
  --  Dialog_Width          -- return the width of the dialog border.
  --  Dialog_Height         -- return the height of the dialog border.
  --
  ----------------------------------------------------------------------------

  function Dialog  (Width   : Positive;
                    Height  : Positive;
                    Title   : String;
                    Command : Command_Type;
                    Font    : Font_Type := Default_Font) return Dialog_Type;

  function Execute (Dialog  : in Dialog_Type) return Command_Type;

  function Dialog_Width   return Natural;
  function Dialog_Height  return Natural;

  ----------------------------------------------------------------------------
  --
  --                             P A N E L S
  --
  --
  --  Panel operations:
  --
  --  Panel (Parent,        -- create a panel inside a parent container, with
  --         Origin,        -- top left coordinates relative to the parent,
  --         Width,         -- with the specified width
  --         Height,        -- and the specified height, and
  --         Title,         -- with this title on the border (default: none)
  --         Font)          -- using this font (default: same as Parent).
  --
  ----------------------------------------------------------------------------

  function Panel (Parent : Container_Type'Class;
                  Origin : Point_Type;
                  Width  : Integer;
                  Height : Integer;
                  Title  : String := "";
                  Font   : Font_Type := Parent_Font) return Panel_Type;

  ----------------------------------------------------------------------------
  --
  --                              M E N U S
  --
  --  A menu is a pull-down list of items attached to a frame's menu bar.
  --  The items on a menu are either menuitems (which generate a command
  --  when they are selected, submenus (which display another menu when
  --  selected) or separators (horizontal bars used to separate a menu
  --  into subsections.
  --
  --  If the text label for a menu contains the character "&", the character
  --  which follows is underlined and the menu can be activated by pressing
  --  Alt + that character. The character "&" is not displayed.
  --
  --  Menu operations:
  --
  --  Menu (Parent,       -- create a menu attached to a frame or a menu
  --        Text)         -- with the specified text label.
  --
  --  Menu_Height         -- return the height of a menu bar.
  --
  ----------------------------------------------------------------------------

  function Menu  (Parent : Frame_Type'Class;
                  Text   : String) return Menu_Type;
  function Menu  (Parent : Menu_Type'Class;
                  Text   : String) return Menu_Type;

  function Menu_Height   return Natural;

  ----------------------------------------------------------------------------
  --
  --                           C O N T R O L S
  --
  --  Controls are windows for user interaction. They hold values (e.g. a
  --  text string) which can normally be set by the user, as well as being
  --  accessed and altered by the program itself. Some controls (e.g. menu
  --  items) generate command values which can be used to trigger actions
  --  in the program. The following operations are common to all controls:
  --
  --  Enable  (Control,     -- enable or disable the control depending on
  --           Enabled)     -- the value of Enabled (default: True).
  --  Disable (Control)     -- disable the control.
  --  Enabled (Control)     -- test if the control is enabled.
  --
  ----------------------------------------------------------------------------

  procedure Enable  (Control : in Control_Type;
                     Enabled : in Boolean := True);
  procedure Disable (Control : in Control_Type);
  function  Enabled (Control : Control_Type) return Boolean;

  ----------------------------------------------------------------------------
  --
  --  The types of control available are as follows:
  --
  --  Menuitem_Type     : controls which can appear on pull-down menus
  --  Text_Control_Type : controls containing with a single-line text string
  --  Multiline_Type    : controls containing multiple text strings
  --  Canvas_Type       : a plain window for drawing arbitrary shapes on
  --
  ----------------------------------------------------------------------------

  type Text_Control_Type  is abstract new Control_Type with private;
  type Multiline_Type     is abstract new Control_Type with private;
  type Canvas_Type        is new Control_Type with private;

  ----------------------------------------------------------------------------
  --
  --                      T E X T   C O N T R O L S
  --
  --  Text controls include a single-line text string. The following
  --  operations are common to all text controls:
  --
  --  Get_Length (Control)  -- get the length of the text associated with the
  --                        -- control.
  --  Get_Text   (Control)  -- get the text associated with the control as a
  --                        -- string of indefinite size.
  --  Get_Text   (Control,  -- get the text associated with the control into
  --              Text,     -- this fixed-size string variable
  --              Length)   -- and set this integer variable to the actual
  --                        -- number of characters copied.
  --  Set_Text   (Control,  -- set the text associated with the control.
  --              Text)     -- to the specified new text value.
  --
  ----------------------------------------------------------------------------

  function  Get_Length (Control : Text_Control_Type) return Natural;
  function  Get_Text   (Control : Text_Control_Type) return String;
  procedure Get_Text   (Control : in  Text_Control_Type;
                        Text    : out String;
                        Length  : out Natural);
  procedure Set_Text   (Control : in  Text_Control_Type;
                        Text    : in  String);

  ----------------------------------------------------------------------------
  --
  --  The types of text control available are as follows:
  --
  --  Button_Type      : a pushbutton which generates a command code.
  --  Label_Type       : a static non-interactive label.
  --  Editbox_Type     : a single-line edit control for text input.
  --
  --  There is a further subclass of text control, as follows:
  --
  --  Boolean_Control_Type : a text control with an associated Boolean state
  --
  ----------------------------------------------------------------------------

  type Button_Type      is new Text_Control_Type with private;
  type Label_Type       is new Text_Control_Type with private;
  type Editbox_Type     is new Text_Control_Type with private;

  type Boolean_Control_Type is abstract new Text_Control_Type with private;

  ----------------------------------------------------------------------------
  --
  --                            B U T T O N S
  --
  --  Buttons are rectangular labelled controls which generate a command
  --  code when pressed. "Default" buttons are displayed with a heavier
  --  border and respond when the Enter key is pressed.
  --
  --  Button operations:
  --
  --  Button (Parent,     -- create a button in a parent container, with
  --          Origin,     -- top left coordinates relative to the parent,
  --          Width,      -- with the specified width
  --          Height,     -- and the specified height,
  --          Text,       -- labelled with the specified text,
  --          Command,    -- generating this command when it is pressed,
  --          Default,    -- set up as a "default" button (default: False),
  --          Font)       -- using this font (default: same as Parent).
  --
  ----------------------------------------------------------------------------

  function Button (Parent  : Container_Type'Class;
                   Origin  : Point_Type;
                   Width   : Integer;
                   Height  : Integer;
                   Text    : String;
                   Command : Command_Type;
                   Default : Boolean := False;
                   Font    : Font_Type := Parent_Font) return Button_Type;

  ----------------------------------------------------------------------------
  --
  --                             L A B E L S
  --
  --  A label is a static text control that can be used to label other
  --  controls. Labels do not respond to user interaction, but their
  --  values can be read and altered by the program in the same way as
  --  any other text control.
  --
  --  Label operations:
  --
  --  Label (Parent,        -- create a label inside a container, with
  --         Origin,        -- top left coordinates relative to the parent,
  --         Width,         -- with the specified width
  --         Height,        -- and the specified height, and
  --         Text,          -- labelled with the specified text
  --         Align,         -- aligned left, right or centre (default: Left),
  --         Font)          -- using this font (default: same as Parent).
  --
  ----------------------------------------------------------------------------

  function Label (Parent : Container_Type'Class;
                  Origin : Point_Type;
                  Width  : Integer;
                  Height : Integer;
                  Text   : String;
                  Align  : Alignment_Type := Left;
                  Font   : Font_Type := Parent_Font) return Label_Type;

  ----------------------------------------------------------------------------
  --
  --                          E D I T B O X E S
  --
  --  An editbox is a text control containing a single line of text whose
  --  value can be edited by the user.
  --
  --  Editbox operations:
  --
  --  Editbox  (Parent,     -- create an editbox inside a container, with
  --            Origin,     -- top left coordinates relative to the parent,
  --            Width,      -- with the specified width
  --            Height,     -- and the specified height, and
  --            Text,       -- initialised with the specified text,
  --            Password,   -- optionally set up as a password editbox,
  --            Font)       -- using this font (default: same as Parent).
  --
  --  Modified (Editbox)    -- test if the editbox has been modified by the
  --                        -- user.
  --
  ----------------------------------------------------------------------------

  function Editbox  (Parent   : Container_Type'Class;
                     Origin   : Point_Type;
                     Width    : Integer;
                     Height   : Integer;
                     Text     : String    := "";
                     Password : Boolean   := False;
                     Font     : Font_Type := Parent_Font) return Editbox_Type;

  function Modified (Editbox  : Editbox_Type) return Boolean;

  ----------------------------------------------------------------------------
  --
  --                   B O O L E A N   C O N T R O L S
  --
  --  Boolean controls are text controls which can be toggled between two
  --  states (checked or unchecked). The following operations are common
  --  to all Boolean controls:
  --
  --  Get_State (Control)   -- get the current state of the control
  --  Set_State (Control,   -- set the current state of the control
  --             State)     -- to the specified value
  --
  ----------------------------------------------------------------------------

  function  Get_State (Control : Boolean_Control_Type) return Boolean;
  procedure Set_State (Control : in Boolean_Control_Type;
                       State   : in Boolean);


  ----------------------------------------------------------------------------
  --
  --  The types of Boolean controls available are as follows:
  --
  --  Checkbox_Type    : a checkbox which can be checked or unchecked.
  --  Radiobutton_Type : a radio button which can be checked or unchecked.
  --
  ----------------------------------------------------------------------------

  type Menuitem_Type    is new Boolean_Control_Type with private;
  type Checkbox_Type    is new Boolean_Control_Type with private;
  type Radiobutton_Type is new Boolean_Control_Type with private;

  ----------------------------------------------------------------------------
  --
  --                         M E N U   I T E M S
  --
  --  Menu items can only be attached to menus. When a menu is clicked, a
  --  pull-down menu appears which can consist of menu items (which generate
  --  a command when clicked) or further menus.
  --
  --  Each menu and menu item has a text label in which the character "&"
  --  is treated specially. "&" will not be displayed, but the following
  --  character (e.g. 'X') will be underlined to indicate that it can be
  --  selected by pressing Alt + 'X' when the menu is visible.
  --
  --  Menuitem operations:
  --
  --  Menuitem  (Parent,    -- create a menu item attached to a parent menu
  --             Text,      -- with this text label,
  --             Command)   -- generating this command code when selected
  --
  --  Separator (Parent)    -- create a separator (an inactive horizontal
  --                        -- bar) attached to a parent menu
  --
  ----------------------------------------------------------------------------

  function Menuitem  (Parent  : Menu_Type'Class;
                      Text    : String;
                      Command : Command_Type)     return Menuitem_Type;

  function Separator (Parent  : Menu_Type'Class)  return Menuitem_Type;

  ----------------------------------------------------------------------------
  --  Menu items behave slightly differently to other controls, so the
  --  following operations are overridden:
  --
  procedure Enable     (Control : in Menuitem_Type;
                        Enabled : in Boolean := True);
  function  Enabled    (Control : Menuitem_Type) return Boolean;
  function  Get_Length (Control : Menuitem_Type) return Natural;
  function  Get_Text   (Control : Menuitem_Type) return String;
  procedure Set_Text   (Control : in Menuitem_Type;
                        Text    : in String);
  function  Get_State  (Control : Menuitem_Type) return Boolean;
  procedure Set_State  (Control : in Menuitem_Type;
                        State   : in Boolean);

  ----------------------------------------------------------------------------
  --
  --                         C H E C K B O X E S
  --
  --  A checkbox is a labelled control with a left-aligned box that can be
  --  checked or unchecked. Clicking on a checkbox (or pressing Space when
  --  it is selected) toggles the checkbox between the checked and unchecked
  --  states.
  --
  --  Checkbox operations:
  --
  --  Checkbox  (Parent,    -- create a checkbox in a container, with
  --             Origin,    -- top left coordinates relative to the parent,
  --             Width,     -- with the specified width
  --             Height,    -- and the specified height, and
  --             Text,      -- labelled with the specified text, and
  --             Checked,   -- set to this initial state (default: False),
  --             Font)      -- using this font (default: same as Parent).
  --
  ----------------------------------------------------------------------------

  function Checkbox (Parent  : Container_Type'Class;
                     Origin  : Point_Type;
                     Width   : Integer;
                     Height  : Integer;
                     Text    : String;
                     Checked : Boolean := False;
                     Font    : Font_Type := Parent_Font) return Checkbox_Type;

  ----------------------------------------------------------------------------
  --
  --                       R A D I O B U T T O N S
  --
  --  A radiobutton is a Boolean control with a left-aligned box that can be
  --  checked or unchecked. Radiobuttons attached to the same container form
  --  a group. Clicking on an unchecked radiobutton will set the radiobutton
  --  to the checked state and will also uncheck the other radiobuttons which
  --  belong to the same group (i.e. which are attached to the same container).
  --  Unlike a checkbox, a radiobutton cannot be turned off by clicking on it;
  --  you can only uncheck a radiobutton by checking another one in the same
  --  group.
  --
  --  Radiobutton operations:
  --
  --  Radiobutton (Parent,      -- create a radiobutton in a container, with
  --               Origin,      -- top left coordinates relative to the parent,
  --               Width,       -- with the specified width
  --               Height,      -- and the specified height, and
  --               Text,        -- labelled with the specified text, and
  --               Checked,     -- set to this initial state (default: False),
  --               Font)        -- using this font (default: Default_Font).
  --
  ----------------------------------------------------------------------------

  function Radiobutton (Parent  : Container_Type'Class;
                        Origin  : Point_Type;
                        Width   : Integer;
                        Height  : Integer;
                        Text    : String;
                        Checked : Boolean := False;
                        Font    : Font_Type := Parent_Font)
                                                    return Radiobutton_Type;

  ----------------------------------------------------------------------------
  --
  --                 M U L T I L I N E   C O N T R O L S
  --
  --  Multiline controls contain multiple lines of text numbered from 1
  --  upwards.  Individual lines can be accessed by specifying a line
  --  number.  The user can select a particular line by clicking on it
  --  with the mouse or using the keyboard arrow keys when the control
  --  is selected.  Specifying the line number to access as 0 will access
  --  the currently selected line.  If no line is selected, the current
  --  line number will be reported as 0 but its contents can still be
  --  accessed .  A Constraint_Error will be raised if an attempt is made
  --  to access a line beyond the last one.
  --
  --  The following operations are common to all multiline controls, but
  --  their precise effects vary slightly according to the actual type of
  --  the control (see below):
  --
  --  Get_Count   (Control) -- get the number of lines of text in the control.
  --  Get_Line    (Control) -- get the number of the currently selected line.
  --  Get_Length  (Control, -- get the length of the specified line
  --               Line)    -- (default: current line).
  --  Get_Text    (Control, -- get the text of the specified line as a string
  --               Line)    -- of indefinite length (default: current line).
  --
  --  Get_Text    (Control, -- get the text of the control
  --               Line,    -- from the specified line (default: current line)
  --               Text,    -- into a fixed-size string variable, together
  --               Length)  -- with the number of characters transferred.
  --  Set_Text    (Control, -- set the text of the control
  --               Line,    -- at the specified line
  --               Text)    -- to the specified value.
  --
  --  Select_Line (Control, -- select the line at the specified line number
  --               Line)    -- (default: 0, meaning deselect).
  --  Append_Line (Control, -- append a line to the end of the control, where
  --               Text)    -- this is the text to append.
  --  Insert_Line (Control, -- insert a new line above the specified
  --               Line,    -- line number, where
  --               Text)    -- this is the text to insert.
  --  Delete_Line (Control, -- delete the specified line.
  --               Line)
  --  Delete_All  (Control) -- delete all lines.
  --
  ----------------------------------------------------------------------------

  function  Get_Count   (Control : Multiline_Type) return Natural is abstract;
  function  Get_Line    (Control : Multiline_Type) return Natural is abstract;
  function  Get_Length  (Control : Multiline_Type;
                         Line    : Natural := 0)   return Natural is abstract;
  function  Get_Text    (Control : Multiline_Type;
                         Line    : Natural := 0)   return String  is abstract;

  procedure Get_Text    (Control : in  Multiline_Type;
                         Line    : in  Natural := 0;
                         Text    : out String;
                         Length  : out Natural);
  procedure Set_Text    (Control : in  Multiline_Type;
                         Text    : in  String;
                         Line    : in  Natural := 0)              is abstract;

  procedure Select_Line (Control : in Multiline_Type;
                         Line    : in Natural := 0)               is abstract;
  procedure Append_Line (Control : in Multiline_Type;
                         Text    : in String)                     is abstract;
  procedure Insert_Line (Control : in Multiline_Type;
                         Text    : in String;
                         Line    : in Natural := 0)               is abstract;
  procedure Delete_Line (Control : in Multiline_Type;
                         Line    : in Natural := 0)               is abstract;
  procedure Delete_All  (Control : in Multiline_Type)             is abstract;

  ----------------------------------------------------------------------------
  --
  --  The types of multiline controls available are as follows:
  --
  --  Listbox_Type  : a list of text items in a scrollable window
  --  Combobox_Type : an editbox in combination with a drop-down list box
  --  Memo_Type     : a multi-line text editor
  --
  ----------------------------------------------------------------------------

  type Listbox_Type  is new Multiline_Type with private;
  type Combobox_Type is new Multiline_Type with private;
  type Memo_Type     is new Multiline_Type with private;

  ----------------------------------------------------------------------------
  --
  --                          L I S T B O X E S
  --
  --  A listbox is a list of lines of text (initially empty). The lines are
  --  sorted into ascending order by default, but can be left unsorted if
  --  required. For a sorted list, the position at which a new line is added
  --  will be ignored, with the new line being inserted at the appropriate
  --  position according to its value. When no line has been selected, the
  --  contents of the current line will be reported as an empty string ("").
  --
  --  Listbox operations:
  --
  --  Listbox (Parent,      -- create a listbox inside a container, with
  --           Origin,      -- top left coordinates relative to the parent,
  --           Width,       -- the specified width
  --           Height,      -- and the specified height, using
  --           Font)        -- this font (default: same as Parent).
  --
  ----------------------------------------------------------------------------

  function Listbox (Parent : Container_Type'Class;
                    Origin : Point_Type;
                    Width  : Integer;
                    Height : Integer;
                    Font   : Font_Type := Parent_Font) return Listbox_Type;

  ----------------------------------------------------------------------------
  --
  --                         C O M B O B O X E S
  --
  --  A combobox consists of an edit control together with a drop-down list
  --  (effectively a combination of an editbox and a listbox). The currently
  --  selected line is displayed in the edit control, and you can specify
  --  whether the user is able to manually edit this value. If not, only
  --  the values in the drop-down list can be selected.
  --
  --  If the contents of the edit control match one of the values in the list,
  --  the position of the corresponding line in the list is reported as the
  --  number of the currently selected line. Otherwise, the number of the
  --  current line is reported as 0. Accessing the value of the current line
  --  (line 0) will report the current value of the edit control.
  --
  --  Combobox operations:
  --
  --  Combobox (Parent,     -- create a combobox inside a container, with
  --            Origin,     -- top left coordinates relative to the parent,
  --            Width,      -- with the specified width, whose value can
  --            Editable,   -- be manually edited (default: True),
  --            Font)       -- using this font (default: same as Parent).
  --
  ----------------------------------------------------------------------------

  function Combobox (Parent   : Container_Type'Class;
                     Origin   : Point_Type;
                     Width    : Integer;
                     Editable : Boolean := True;
                     Font     : Font_Type := Parent_Font) return Combobox_Type;

  ----------------------------------------------------------------------------
  --
  --                              M E M O S
  --
  --  A memo is a simple multi-line text editor similar to Windows Notepad.
  --  There are several memo-specific operations in addition to the standard
  --  operations on multi-line controls. The user can select a block of text
  --  spanning multiple lines using the mouse (or by moving the cursor with
  --  the Shift key held down) and there are operations to fetch, replace
  --  and delete the selected text, find the line and column position of
  --  the start and end of the selected text, and get its total length (which
  --  will include one or more additional end-of-line characters if the text
  --  spans more than one line).
  --
  --  Memo operations:
  --
  --  Memo (Parent,           -- create a memo inside a container, with
  --        Origin,           -- top left coordinates relative to the parent,
  --        Width,            -- the specified width
  --        Height)           -- and the specified height
  --        Font)             -- using this font (default: same as Parent).
  --
  --  Get_Column      (Memo)  -- get the column position of the current
  --                          -- selection.
  --  Modified        (Memo)  -- test if the user has modified the memo.
  --  Cut_Selection   (Memo)  -- cut the current selection to the clipboard.
  --  Copy_Selection  (Memo)  -- copy the current selection to the clipboard.
  --  Paste_Selection (Memo)  -- paste the clipboard contents to the memo,
  --                          -- replacing the current selection.
  --  Undo_Change     (Memo)  -- undo the user's last change to the memo.
  --  Show_Selection  (Memo)  -- scroll the memo so that the current position
  --                          -- is visible on the screen.
  --
  ----------------------------------------------------------------------------

  function  Memo (Parent : Container_Type'Class;
                  Origin : Point_Type;
                  Width  : Integer;
                  Height : Integer;
                  Font   : Font_Type := Parent_Font) return Memo_Type;

  function  Get_Column      (Memo : Memo_Type) return Natural;
  function  Modified        (Memo : Memo_Type) return Boolean;
  procedure Cut_Selection   (Memo : in Memo_Type);
  procedure Copy_Selection  (Memo : in Memo_Type);
  procedure Paste_Selection (Memo : in Memo_Type);
  procedure Undo_Change     (Memo : in Memo_Type);
  procedure Show_Selection  (Memo : in Memo_Type);

  ----------------------------------------------------------------------------
  --  Most operations on multiline types are implemented in different ways,
  --  so they are overridden here:
  --
  function  Get_Count   (Control : Listbox_Type)  return Natural;
  function  Get_Count   (Control : Combobox_Type) return Natural;
  function  Get_Count   (Control : Memo_Type)     return Natural;

  function  Get_Line    (Control : Listbox_Type)  return Natural;
  function  Get_Line    (Control : Combobox_Type) return Natural;
  function  Get_Line    (Control : Memo_Type)     return Natural;

  function  Get_Length  (Control : Listbox_Type;
                         Line    : Natural := 0)  return Natural;
  function  Get_Length  (Control : Combobox_Type;
                         Line    : Natural := 0)  return Natural;
  function  Get_Length  (Control : Memo_Type;
                         Line    : Natural := 0)  return Natural;

  function  Get_Text    (Control : Listbox_Type;
                         Line    : Natural := 0)  return String;
  function  Get_Text    (Control : Combobox_Type;
                         Line    : Natural := 0)  return String;
  function  Get_Text    (Control : Memo_Type;
                         Line    : Natural := 0)  return String;

  procedure Set_Text    (Control : in  Listbox_Type;
                         Text    : in  String;
                         Line    : in  Natural := 0);
  procedure Set_Text    (Control : in  Combobox_Type;
                         Text    : in  String;
                         Line    : in  Natural := 0);
  procedure Set_Text    (Control : in  Memo_Type;
                         Text    : in  String;
                         Line    : in  Natural := 0);

  procedure Select_Line (Control : in  Listbox_Type;
                         Line    : in  Natural := 0);
  procedure Select_Line (Control : in  Combobox_Type;
                         Line    : in  Natural := 0);
  procedure Select_Line (Control : in  Memo_Type;
                         Line    : in  Natural := 0);

  procedure Append_Line (Control : in Listbox_Type;
                         Text    : in String);
  procedure Append_Line (Control : in Combobox_Type;
                         Text    : in String);
  procedure Append_Line (Control : in Memo_Type;
                         Text    : in String);

  procedure Insert_Line (Control : in Listbox_Type;
                         Text    : in String;
                         Line    : in Natural := 0);
  procedure Insert_Line (Control : in Combobox_Type;
                         Text    : in String;
                         Line    : in Natural := 0);
  procedure Insert_Line (Control : in Memo_Type;
                         Text    : in String;
                         Line    : in Natural := 0);

  procedure Delete_Line (Control : in Listbox_Type;
                         Line    : in Natural := 0);
  procedure Delete_Line (Control : in Combobox_Type;
                         Line    : in Natural := 0);
  procedure Delete_Line (Control : in Memo_Type;
                         Line    : in Natural:= 0);

  procedure Delete_All  (Control : in Listbox_Type);
  procedure Delete_All  (Control : in Combobox_Type);
  procedure Delete_All  (Control : in Memo_Type);

  ----------------------------------------------------------------------------
  --
  --                           C A N V A S E S
  --
  --  A canvas is a blank rectangle for drawing on which can optionally
  --  be set to generate a command code when the mouse is clicked on it.
  --  A canvas has an associated font (the same as the parent window by
  --  default), a background colour (initially white), a pen for drawing
  --  lines (initially black, one pixel wide) and a fill colour used to
  --  colour closed shapes (initially white).
  --
  --  The freedom of expression available with canvases make these the
  --  most complex component of all, with over 20 available operations.
  --  There are operations to draw lines, rectangles (with or without
  --  rounded corners), ellipses, circles, line sequences, polygons and
  --  text. The font, pen size and colour and fill colour can be changed
  --  at any time any will affect all subsequent drawing operations (but
  --  everything drawn previously will be unaffected). Rectangles can be
  --  given rounded corners by specifying a rounding factor, which gives
  --  the X and Y offsets from the corners to the points where the rounded
  --  corner begins.
  --
  --  Anything drawn on a canvas will normally stay there, but the canvas
  --  can be erased or the current state of a drawing can be saved and then
  --  restored later (which provides a basic "undo" facility). For example,
  --  a clock can be drawn by drawing a circle, saving the drawing, and then
  --  drawing the hands. To change the position of the hands, restore the
  --  saved drawing (thus erasing the hands) and then redraw the hands in
  --  the new position. You can only save one copy of the drawing, so if you
  --  save it a second time you will lose the previous saved copy.
  --
  --  A canvas can be set up to generate a command when the mouse button
  --  is pressed inside its borders. There are operations to get the
  --  position at which the button was pressed, to get the current mouse
  --  position, and to test if the mouse button is still down and whether
  --  the mouse has been moved. As long as the button is down, the mouse
  --  position will be tracked even if the mouse is moved outside the canvas.
  --  You can track the mouse visually by saving the drawing when the mouse
  --  is first pressed, then restoring the drawing and drawing a new line
  --  from the initial mouse position to the current one.
  --
  --  Canvas operations:
  --
  --  Canvas  (Parent,            -- create a canvas inside a container, with
  --           Origin,            -- top left coordinates relative to Parent,
  --           Width,             -- the specified width
  --           Height,            -- and the specified height, using
  --           Font)              -- this font (default: same as Parent).
  --
  --  Canvas  (Parent,            -- create a canvas inside a container, with
  --           Origin,            -- top left coordinates relative to the parent,
  --           Width,             -- the specified width
  --           Height,            -- and the specified height,
  --           Command,           -- which generates this command code, and
  --           Font)              -- uses this font (default: same as Parent).
  --
  --  Canvas  (Parent,            -- create a canvas inside a container, with
  --           Origin,            -- top left coordinates relative to the parent,
  --           Width,             -- the specified width
  --           Height,            -- and the specified height,
  --           Command,           -- which generates this command code, and
  --           Keypress,          -- this one when a key is pressed, and
  --           Font)              -- uses this font (default: same as Parent).
  --
  --  Set_Colour     (Canvas,     -- set the background colour for a canvas
  --                  Colour)     -- using this colour (default: white)
  --
  --  Erase          (Canvas)     -- erase the drawing in a canvas. This will
  --                              -- also erase any saved drawing.
  --  Save           (Canvas)     -- save the current drawing.
  --  Restore        (Canvas)     -- restore a saved drawing, erasing anything
  --                              -- drawn since. If the drawing has never been
  --                              -- saved, or the window has been erased using
  --                              -- Erase, this will do nothing.
  --  Set_Font       (Canvas,     -- set a new font for all subsequent text
  --                  Font)       -- drawn on the canvas.
  --  Set_Pen        (Canvas,     -- set the pen used to draw lines on the
  --                  Colour,     -- canvas to this colour (default: black)
  --                  Width)      -- and thickness (default: 1 pixel).
  --  Set_Fill       (Canvas,     -- set the colour used to fill subsequent
  --                  Colour)     -- closed shapes drawn on the canvas.
  --  Set_Fill       (Canvas)     -- clear the colour used to fill subsequent
  --                              -- closed shapes drawn on the canvas.
  --  Draw_Text      (Canvas,     -- draw a text string on the canvas
  --                  From,       -- from this top-left point, where
  --                  Text)       -- this is the text to be drawn.
  --  Draw_Text      (Canvas,     -- draw a text string on the canvas
  --                  From,       -- in a rectangle between this point
  --                  To,         -- and this one, where
  --                  Text,       -- this is the text to be drawn
  --                  Align)      -- with this alignment (default: left).
  --  Draw_Text      (Canvas,     -- draw a text string on the canvas
  --                  From,       -- in a rectangle starting at this point
  --                  Width,      -- with this width
  --                  Height,     -- and this height, where
  --                  Text,       -- this is the text to be drawn
  --                  Align)      -- with this alignment (default: left).
  --
  --  Draw_Line      (Canvas,     -- draw a line on the canvas
  --                  From,       -- from this point
  --                  To)         -- to this one.
  --  Draw_Line      (Canvas,     -- draw a line on the canvas
  --                  From,       -- from this point
  --                  Length,     -- for this length
  --                  Angle)      -- at this angle.
  --  Draw_Line_List (Canvas,     -- draw lines on the canvas connecting
  --                  Points)     -- the points in this list.
  --
  --  Draw_Rectangle (Canvas,     -- draw a rectangle on the canvas
  --                  From,       -- from this corner point
  --                  To,         -- to this diagonally-opposite point
  --                  Rounding)   -- with corners rounded this much
  --                              -- (default: no rounding).
  --  Draw_Rectangle (Canvas,     -- draw a rectangle on the canvas
  --                  From,       -- from this top-left point
  --                  Width,      -- with this width
  --                  Height,     -- and this height
  --                  Rounding)   -- with corners rounded this much
  --                              -- (default: no rounding).
  --
  --  Draw_Ellipse   (Canvas,     -- draw an ellipse on the canvas
  --                  From,       -- bounded by a rectangle from this point
  --                  To)         -- to this point.
  --  Draw_Ellipse   (Canvas,     -- draw an ellipse on the canvas
  --                  From,       -- bounded by a rectangle from this point
  --                  Width,      -- with this width.
  --                  Height)     -- and this height
  --  Draw_Circle    (Canvas,     -- draw a circle on the canvas
  --                  Centre,     -- with this centre point
  --                  Radius)     -- and this radius.
  --
  --  Draw_Polygon   (Canvas,     -- draw a polygon on the canvas
  --                  Points)     -- with vertices at these points.
  --
  --  Draw_Image     (Canvas,     -- draw an image on the canvas
  --                  From,       -- from this point
  --                  Image)      -- using this image object.
  --  Draw_Image     (Canvas,     -- draw an image on the canvas
  --                  From,       -- from this point
  --                  To,         -- to this one (stretching the image to fit)
  --                  Image)      -- using this image object.
  --  Draw_Image     (Canvas,     -- draw an image on the canvas
  --                  From,       -- from this point
  --                  Width,      -- with this width
  --                  Height,     -- and this height
  --                  Image)      -- using this image object.
  --
  --  Mouse_Down     (Canvas)     -- test if the left mouse button is down.
  --  Mouse_Moved    (Canvas)     -- test if the mouse has been moved.
  --  Start_Point    (Canvas)     -- get the point where the mouse button
  --                              -- was first pressed.
  --  End_Point      (Canvas)     -- get the point where the mouse is now
  --                              -- (or its final position when the left
  --                              -- button was released).
  --  Key_Code       (Canvas)     -- get the character code for the last
  --                              -- key pressed.
  --
  ----------------------------------------------------------------------------

  function Canvas (Parent   : Container_Type'Class;
                   Origin   : Point_Type;
                   Width    : Integer;
                   Height   : Integer;
                   Font     : Font_Type := Parent_Font) return Canvas_Type;

  function Canvas (Parent   : Container_Type'Class;
                   Origin   : Point_Type;
                   Width    : Integer;
                   Height   : Integer;
                   Command  : Command_Type;
                   Font     : Font_Type := Parent_Font) return Canvas_Type;

  function Canvas (Parent   : Container_Type'Class;
                   Origin   : Point_Type;
                   Width    : Integer;
                   Height   : Integer;
                   Command  : Command_Type;
                   Keypress : Command_Type;
                   Font     : Font_Type := Parent_Font) return Canvas_Type;

  procedure Set_Colour (Canvas : in Canvas_Type;
                        Colour : in Colour_Type := White);

  procedure Erase      (Canvas   : in Canvas_Type);
  procedure Save       (Canvas   : in Canvas_Type);
  procedure Restore    (Canvas   : in Canvas_Type);

  procedure Set_Font   (Canvas   : in Canvas_Type;
                        Font     : in Font_Type);
  procedure Set_Pen    (Canvas   : in Canvas_Type;
                        Colour   : in Colour_Type := Black;
                        Width    : in Natural     := 1);
  procedure Set_Fill   (Canvas   : in Canvas_Type;
                        Colour   : in Colour_Type);
  procedure Set_Fill   (Canvas   : in Canvas_Type);

  procedure Draw_Text  (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Text     : in String);
  procedure Draw_Text  (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        To       : in Point_Type;
                        Text     : in String;
                        Align    : in Alignment_Type := Left);
  procedure Draw_Text  (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Width    : in Integer;
                        Height   : in Integer;
                        Text     : in String;
                        Align    : in Alignment_Type := Left);

  procedure Draw_Line  (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        To       : in Point_Type);
  procedure Draw_Line  (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Length   : in Positive;
                        Angle    : in Angle_Type);

  procedure Draw_Line_List
                       (Canvas   : in Canvas_Type;
                        Points   : in Point_List);

  procedure Draw_Rectangle
                       (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        To       : in Point_Type;
                        Rounding : in Point_Type := (0,0));
  procedure Draw_Rectangle
                       (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Width    : in Positive;
                        Height   : in Positive;
                        Rounding : in Point_Type := (0,0));

  procedure Draw_Ellipse
                       (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        To       : in Point_Type);
  procedure Draw_Ellipse
                       (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Width    : in Positive;
                        Height   : in Positive);
  procedure Draw_Circle
                       (Canvas   : in Canvas_Type;
                        Centre   : in Point_Type;
                        Radius   : in Positive);

  procedure Draw_Polygon
                       (Canvas   : in Canvas_Type;
                        Points   : in Point_List);

  procedure Draw_Image (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Image    : in Image_Type);
  procedure Draw_Image (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        To       : in Point_Type;
                        Image    : in Image_Type);
  procedure Draw_Image (Canvas   : in Canvas_Type;
                        From     : in Point_Type;
                        Width    : in Natural;
                        Height   : in Natural;
                        Image    : in Image_Type);

  function Start_Point (Canvas   : Canvas_Type) return Point_Type;
  function End_Point   (Canvas   : Canvas_Type) return Point_Type;
  function Mouse_Down  (Canvas   : Canvas_Type) return Boolean;
  function Mouse_Moved (Canvas   : Canvas_Type) return Boolean;
  function Key_Code    (Canvas   : Canvas_Type) return Character;

  ----------------------------------------------------------------------------
  --
  --                     C O M M O N   D I A L O G S
  --
  --  Common dialogs are pre-packaged dialog widgets which are not treated
  --  as normal windows (although they are made to look similar for ease of
  --  use).
  --
  --  Common dialog operations (common to all dialogs):
  --
  --  Execute (Dialog)      -- execute the dialog and return True if the OK
  --                        -- button was used to close the dialog and False
  --                        -- otherwise.
  --
  ----------------------------------------------------------------------------

  type Common_Dialog_Type is abstract tagged private;

  function Execute (Dialog : Common_Dialog_Type) return Boolean;
                                
  ----------------------------------------------------------------------------
  --
  --           C O M M O N   D I A L O G   S U B C L A S S E S
  --
  --  The available dialog subclasses are colour, font and file dialogs.
  --
  --  Colour_Dialog_Type : a dialog to allow the user to select a colour.
  --  Font_Dialog_Type   : a dialog to allow the user to select a font.
  --  File_Dialog_Type   : a dialog to allow the user to select a file name.
  --
  ----------------------------------------------------------------------------

  type Colour_Dialog_Type is new Common_Dialog_Type with private;
  type Font_Dialog_Type   is new Common_Dialog_Type with private;
  type File_Dialog_Type   is abstract new Common_Dialog_Type with private;
  
  ----------------------------------------------------------------------------
  --
  --                     C O L O U R   D I A L O G S
  --
  --  Colour dialogs allow the user to select or create a colour.
  --
  --  Colour dialog operations:
  --
  --  Colour_Dialog           -- create a colour dialog.
  --  Set_Colour    (Dialog,  -- set the initial colour displayed in the
  --                 Colour)  -- dialog to this colour.
  --  Get_Colour    (Dialog)  -- get the colour contained in the dialog.
  --
  ----------------------------------------------------------------------------

  function  Colour_Dialog return Colour_Dialog_Type;

  procedure Set_Colour    (Dialog : in Colour_Dialog_Type;
                           Colour : in Colour_Type);
  function  Get_Colour    (Dialog : in Colour_Dialog_Type) return Colour_Type;

  ----------------------------------------------------------------------------
  --
  --                       F O N T   D I A L O G S
  --
  --  Font dialogs allow the user to select a font.
  --
  --  Font dialog operations:
  --
  --  Font_Dialog             -- create a font dialog.
  --  Set_Font    (Dialog,    -- set the initial font displayed in the
  --               Font)      -- dialog to this font.
  --  Get_Font    (Dialog)    -- get the font contained in the dialog.
  --
  ----------------------------------------------------------------------------

  function  Font_Dialog return Font_Dialog_Type;
  
  procedure Set_Font    (Dialog : in Font_Dialog_Type;
                         Font   : in Font_Type);
  function  Get_Font    (Dialog : in Font_Dialog_Type) return Font_Type;

  ----------------------------------------------------------------------------
  --
  --                       F I L E   D I A L O G S
  --
  --  File dialogs allow the user to select or enter a file name. This is an
  --  abstract type which is further subclassed below.
  --
  --  File dialog operations (common to all file dialogs):
  --
  --  Set_Name      (Dialog,    -- set the initial file name displayed in the
  --                 Name)      -- dialog to this string.
  --  Get_Name      (Dialog)    -- get the file name contained in the dialog.
  --  Add_Filter    (Dialog,    -- add a file type filter to the dialog
  --                 Text,      -- with this description
  --                 Filter)    -- to match this wildcard specification.
  --  Set_Directory (Dialog,    -- set the initial directory for the dialog
  --                 Name)      -- to this directory.
  --
  ----------------------------------------------------------------------------

  procedure Set_Name      (Dialog : in File_Dialog_Type;
                           Name   : in String);
  function  Get_Name      (Dialog : in File_Dialog_Type) return String;

  procedure Add_Filter    (Dialog : in File_Dialog_Type;
                           Text   : in String;
                           Filter : in String);

  procedure Set_Directory (Dialog : in File_Dialog_Type;
                           Name   : in String);

  ----------------------------------------------------------------------------
  --
  --                       O P E N   D I A L O G S
  --
  --  Open dialogs allow the user to select or enter a file name for use as
  --  an input file. The file name selected must be the name of an existing
  --  file.
  --
  --  Open dialog operations:
  --
  --  Open_Dialog (Title)     -- create an open file dialog with this title.
  --
  ----------------------------------------------------------------------------

  type Open_Dialog_Type is new File_Dialog_Type with private;

  function Open_Dialog (Title  : String) return Open_Dialog_Type;

  ----------------------------------------------------------------------------
  --
  --                       S A V E   D I A L O G S
  --
  --  Save dialogs allow the user to select or enter a file name for use as
  --  an output file. If the Create parameter to the constructor function
  --  below is True (as it is by default) and an existing file is selected,
  --  the user will be asked if the file should be overwritten. If it is
  --  False and the file does not exist, the user will be asked if it should
  --  be created.
  --
  --  Save dialog operations:
  --
  --  Save_Dialog (Title,     -- create a save file dialog with this title
  --               Create)    -- which will prompt the user as described above.
  --
  ----------------------------------------------------------------------------

  type Save_Dialog_Type is new File_Dialog_Type with private;

  function Save_Dialog (Title  : String;
                        Create : Boolean := True) return Save_Dialog_Type;

  ----------------------------------------------------------------------------
  --  Renamings for our transatlantic cousins, in the hope that some day
  --  they'll repay the favour/favor...
  ----------------------------------------------------------------------------

  procedure Set_Color (Canvas : in Canvas_Type;
                       Colour : in Colour_Type := White) renames Set_Colour;

  subtype   Color_Dialog_Type is Colour_Dialog_Type;

  function  Color_Dialog  return Colour_Dialog_Type renames Colour_Dialog;
  procedure Set_Color    (Dialog : in Colour_Dialog_Type;
                          Colour : in Colour_Type) renames Set_Colour;
  function  Get_Color    (Dialog : in Colour_Dialog_Type) return Colour_Type
                                                   renames Get_Colour;

private       -- and deliberately uninformative!

  type Window_Type is abstract tagged
    record
      Internals : JEWL.Controlled_Type;   -- see package JEWL
    end record;

  type Container_Type       is abstract new Window_Type with null record;
  type Control_Type         is abstract new Window_Type with null record;

  type Frame_Type           is new Container_Type with null record;
  type Dialog_Type          is new Container_Type with null record;
  type Panel_Type           is new Container_Type with null record;
  type Menu_Type            is new Container_Type with null record;

  type Text_Control_Type    is abstract new Control_Type with null record;
  type Multiline_Type       is abstract new Control_Type with null record;
  type Canvas_Type          is new Control_Type with null record;

  type Button_Type          is new Text_Control_Type with null record;
  type Label_Type           is new Text_Control_Type with null record;
  type Editbox_Type         is new Text_Control_Type with null record;
  type Boolean_Control_Type is abstract new Text_Control_Type with null record;

  type Menuitem_Type        is new Boolean_Control_Type with null record;
  type Checkbox_Type        is new Boolean_Control_Type with null record;
  type Radiobutton_Type     is new Boolean_Control_Type with null record;

  type Listbox_Type         is new Multiline_Type with null record;
  type Combobox_Type        is new Multiline_Type with null record;
  type Memo_Type            is new Multiline_Type with null record;

  type Common_Dialog_Type is abstract tagged
    record
      Internals : JEWL.Controlled_Type;   -- see package JEWL
    end record;

  type Colour_Dialog_Type is new Common_Dialog_Type with null record;
  type Font_Dialog_Type   is new Common_Dialog_Type with null record;
  type File_Dialog_Type   is abstract new Common_Dialog_Type with null record;

  type Open_Dialog_Type   is new File_Dialog_Type with null record;
  type Save_Dialog_Type   is new File_Dialog_Type with null record;

  type Image_Type is
    record
      Internals : JEWL.Controlled_Type;   -- see package JEWL
    end record;

end JEWL.Windows;
------------------------------------------------------------------------------
--                                                                          --
--           J E W L . W I N D O W _ I M P L E M E N T A T I O N            --
--                                                                          --
--   This is the body of a private package containing implementation        --
--   details for JEWL.Windows. It defines the cleanup procedures which      --
--   are called automatically to destroy a window's Window_Internals        --
--   structure when the last reference to the window is deleted.            --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-window_implementation.adb 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-window_implementation.adb $
-- Revision 1.7  2007/01/08 17:00:00  JE
-- * Fixed linker options in JEWL.Win32_Interface to accommodate changes to GNAT
--   GPL 2006 compiler (thanks to John McCormick for this)
-- * Added delay in message loop to avoid the appearance of hogging 100% of CPU
--   time
--
-- Revision 1.6  2001/11/02 16:00:00  JE
-- * Fixed canvas bug when saving an empty canvas
-- * Restore with no prior save now acts as erase
-- * Removed redundant variable declaration in Image function
--
-- Revision 1.5  2001/08/22 15:00:00  JE
-- * Minor bugfix to Get_Text for combo boxes
-- * Minor changes to documentation (including new example involving dialogs)
--
-- Revision 1.4  2001/01/25 09:00:00  je
-- Changes visible to the user:
--
-- * Added support for drawing bitmaps on canvases (Draw_Image operations
--   and new type Image_Type)
-- * Added Play_Sound
-- * Added several new operations on all windows: Get_Origin, Get_Width,
--   Get_Height, Set_Origin, Set_Size and Focus
-- * Added several functions giving screen and window dimensions: Screen_Width,
--   Screen_Height, Frame_Width, Frame_Height, Dialog_Width, Dialog_Height and
--   Menu_Height
-- * Canvases can now handle keyboard events: new constructor and Key_Code added
-- * Added procedure Play_Sound
-- * Operations "+" and "-" added for Point_Type
-- * Pens can now be zero pixels wide
-- * The absolute origin of a frame can now have be specified when the frame
--   is created
-- * Added new File_Dialog operations Add_Filter and Set_Directory
-- * Added Get_Line renames to JEWL.IO for compatibility with Ada.Text_IO
-- * Added all the Get(File,Item) operations mentioned in documentation but
--   unaccountably missing :-(
-- * Documentation updated to reflect the above changes
-- * HTML versions of public package specifications added with links from
--   main documentation pages
--
-- Other internal changes:
--
-- * Canvas fonts, pens etc. now use JEWL.Reference_Counted_Type rather than
--   reinventing this particular wheel, as do images
-- * Various minor code formatting changes: some code reordered for clarity,
--   some comments added or amended,
-- * Changes introduced in 1.2 to support GNAT 3.10 have been reversed, since
--   GNAT 3.10 still couldn't compile this code correctly... ;-(
--
-- Outstanding issues:
--
-- * Optimisation breaks the code (workaround: don't optimise)
--
-- Revision 1.3  2000/07/07 12:00:00  je
-- * JEWL.Simple_Windows added; JEWL.IO modified to use JEWL.Simple_Windows.
-- * JEWL.IO bug fix: Put_Line to file wrote newline to standard output
--   instead of to the file (thanks to Jeff Carter for pointing this out).
-- * Panels fixed so that mouse clicks are passed on correctly to subwindows.
-- * Memos fixed so that tabs are handled properly.
-- * Password feature added to editboxes.
-- * Minor typos fixed in comments within the package sources.
-- * Documentation corrected and updated following comments from Moti Ben-Ari
--   and Don Overheu.
--
-- Revision 1.2  2000/04/18 20:00:00  je
-- * Minor code changes to enable compilation by GNAT 3.10
-- * Minor documentation errors corrected
-- * Some redundant "with" clauses removed
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

with JEWL.Message_Handling;

package body JEWL.Window_Implementation is

  use JEWL.Canvas_Implementation;
  use JEWL.Message_Handling;
  use JEWL.Win32_Interface;

  use type System.Address;
  use type Win32_BOOL, Win32_DWORD;

  ----------------------------------------------------------------------------
  --
  --  Cleanup: destructor for Window_Internals.
  --
  --  This destroys the font handle associated with the window.
  --
  ----------------------------------------------------------------------------

  procedure Cleanup (Object : in out Window_Internals) is
  begin
    if Object.Font /= System.Null_Address then
      Bool_Dummy := DeleteObject (Object.Font);
    end if;
  end Cleanup;

  ----------------------------------------------------------------------------
  --
  --  Cleanup: destructor for Main_Window_Internals.
  --
  --  This asks the message loop to destroy the window if it still exists to
  --  ensure that the count of top-level windows is decremented properly.
  --
  ----------------------------------------------------------------------------

  procedure Cleanup (Object : in out Main_Window_Internals) is
  begin
    if IsWindow(Object.Handle) /= 0 then
      Message_Loop.Destroy_Window (Object.Handle);
    end if;
    Cleanup (Window_Internals(Object));
  end Cleanup;

  ----------------------------------------------------------------------------
  --
  --  Cleanup: destructor for Canvas_Internals.
  --
  --  This deletes the drawing list associated with the canvas.
  --
  ----------------------------------------------------------------------------

  procedure Cleanup (Object : in out Canvas_Internals) is
  begin
    if IsWindow(Object.Handle) /= 0 then
      Object.Monitor.Clear;
    end if;
    Cleanup (Window_Internals(Object));
  end Cleanup;

  ----------------------------------------------------------------------------
  --
  --           C O M M O N   D I A L O G   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Show_Dialog: show a colour dialog.
  --
  function Show_Dialog (Dialog : access Colour_Dialog_Internals)
                                                      return Boolean is
    B : Win32_BOOL;
  begin
    Dialog.Struct.rgbResult := RGB (Dialog.Colour);
    Dialog.Struct.hwndOwner := GetActiveWindow;
    B := ChooseColor (Dialog.Struct'Access);
    if B /= 0 then
      Dialog.Colour.Red   := Integer(Dialog.Struct.rgbResult mod 256);
      Dialog.Colour.Green := Integer((Dialog.Struct.rgbResult/2**8) mod 256);
      Dialog.Colour.Blue  := Integer((Dialog.Struct.rgbResult/2**16) mod 256);
    end if;
    return B /= 0;
  end Show_Dialog;

  ----------------------------------------------------------------------------
  --
  --  Show_Dialog: show a font dialog.
  --
  function Show_Dialog (Dialog : access Font_Dialog_Internals)
                                                    return Boolean is
  begin
    Dialog.Struct.hwndOwner := GetActiveWindow;
    return ChooseFont (Dialog.Struct'Access) /= 0;
  end Show_Dialog;

  ----------------------------------------------------------------------------
  --
  --  Show_Dialog: show an open file dialog.
  --
  function Show_Dialog (Dialog : access Open_Dialog_Internals)
                                                    return Boolean is
  begin
    Dialog.Struct.hwndOwner := GetActiveWindow;
    return GetOpenFileName(Dialog.Struct'Access) /= 0;
  end Show_Dialog;

  ----------------------------------------------------------------------------
  --
  --  Show_Dialog: show a save file dialog.
  --
  function Show_Dialog (Dialog : access Save_Dialog_Internals)
                                                    return Boolean is
  begin
    Dialog.Struct.hwndOwner := GetActiveWindow;
    return GetSaveFileName(Dialog.Struct'Access) /= 0;
  end Show_Dialog;

end JEWL.Window_Implementation;
------------------------------------------------------------------------------
--                                                                          --
--           J E W L . W I N D O W _ I M P L E M E N T A T I O N            --
--                                                                          --
--   This is a private package containing implementation details for        --
--   JEWL.Windows. Because this package is non-generic, the type            --
--   Window_Internals can be defined here at library level by deriving      --
--   from JEWL.Reference_Counted_Type, thus avoiding scope problems         --
--   arising from the use of a controlled type in a generic package         --
--   (which would otherwise have to be instantiated at library level).      --
--   Besides, JEWL.Windows is far too big anyway...                         --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl-window_implementation.ads 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl-window_implementation.ads $
-- Revision 1.7  2007/01/08 17:00:00  JE
-- * Fixed linker options in JEWL.Win32_Interface to accommodate changes to GNAT
--   GPL 2006 compiler (thanks to John McCormick for this)
-- * Added delay in message loop to avoid the appearance of hogging 100% of CPU
--   time
--
-- Revision 1.6  2001/11/02 16:00:00  JE
-- * Fixed canvas bug when saving an empty canvas
-- * Restore with no prior save now acts as erase
-- * Removed redundant variable declaration in Image function
--
-- Revision 1.5  2001/08/22 15:00:00  JE
-- * Minor bugfix to Get_Text for combo boxes
-- * Minor changes to documentation (including new example involving dialogs)
--
-- Revision 1.4  2001/01/25 09:00:00  je
-- Changes visible to the user:
--
-- * Added support for drawing bitmaps on canvases (Draw_Image operations
--   and new type Image_Type)
-- * Added Play_Sound
-- * Added several new operations on all windows: Get_Origin, Get_Width,
--   Get_Height, Set_Origin, Set_Size and Focus
-- * Added several functions giving screen and window dimensions: Screen_Width,
--   Screen_Height, Frame_Width, Frame_Height, Dialog_Width, Dialog_Height and
--   Menu_Height
-- * Canvases can now handle keyboard events: new constructor and Key_Code added
-- * Added procedure Play_Sound
-- * Operations "+" and "-" added for Point_Type
-- * Pens can now be zero pixels wide
-- * The absolute origin of a frame can now have be specified when the frame
--   is created
-- * Added new File_Dialog operations Add_Filter and Set_Directory
-- * Added Get_Line renames to JEWL.IO for compatibility with Ada.Text_IO
-- * Added all the Get(File,Item) operations mentioned in documentation but
--   unaccountably missing :-(
-- * Documentation updated to reflect the above changes
-- * HTML versions of public package specifications added with links from
--   main documentation pages
--
-- Other internal changes:
--
-- * Canvas fonts, pens etc. now use JEWL.Reference_Counted_Type rather than
--   reinventing this particular wheel, as do images
-- * Various minor code formatting changes: some code reordered for clarity,
--   some comments added or amended,
-- * Changes introduced in 1.2 to support GNAT 3.10 have been reversed, since
--   GNAT 3.10 still couldn't compile this code correctly... ;-(
--
-- Outstanding issues:
--
-- * Optimisation breaks the code (workaround: don't optimise)
--
-- Revision 1.3  2000/07/07 12:00:00  je
-- * JEWL.Simple_Windows added; JEWL.IO modified to use JEWL.Simple_Windows.
-- * JEWL.IO bug fix: Put_Line to file wrote newline to standard output
--   instead of to the file (thanks to Jeff Carter for pointing this out).
-- * Panels fixed so that mouse clicks are passed on correctly to subwindows.
-- * Memos fixed so that tabs are handled properly.
-- * Password feature added to editboxes.
-- * Minor typos fixed in comments within the package sources.
-- * Documentation corrected and updated following comments from Moti Ben-Ari
--   and Don Overheu.
--
-- Revision 1.2  2000/04/18 20:00:00  je
-- * Minor code changes to enable compilation by GNAT 3.10
-- * Minor documentation errors corrected
-- * Some redundant "with" clauses removed
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

with JEWL.Canvas_Implementation;  use JEWL.Canvas_Implementation;
with JEWL.Win32_Interface;        use JEWL.Win32_Interface;
with System;

private package JEWL.Window_Implementation is

  type Window_Internals;
  type Window_Ptr is access all Window_Internals'Class;

  type Container_Internals;
  type Container_Ptr is access all Container_Internals;

  ----------------------------------------------------------------------------
  --
  --                   W I N D O W _ I N T E R N A L S
  --
  --  Window_Internals (or a type derived from Window_Internals) is what
  --  the Controlled_Type object in every Window_Type object actually
  --  points to. It contains the following fields:
  --
  --  Handle  : the native Windows handle for the window
  --  Parent  : the parent window (null for top-level windows)
  --  Next    : link to the next sibling of this window
  --  First   : link to the first child window of this window
  --  Last    : link to the last child window
  --  Action  : the command code associated with window (-1 if none)
  --  Font    : font handle for the window's font
  --  Top     : position of top of window (negative if relative to parent)
  --  Left    : position of top of window (negative if relative to parent)
  --  Height  : position of top of window (non-positive if relative to parent)
  --  Width   : position of top of window (non-positive if relative to parent)
  --  WndProc : old window procedure for subclassed windows
  --
  --  The Cleanup procedure is called automatically when a Window_Internals
  --  object is deleted and should not be called directly.
  --
  ----------------------------------------------------------------------------

  type Window_Internals is new Reference_Counted_Type with
    record
      Handle      : Win32_HWND := System.Null_Address;
      Parent      : Container_Ptr;
      Next        : Controlled_Type;
      First       : Controlled_Type;
      Last        : Window_Ptr;
      Action      : Integer := -1;
      Font        : Win32_HFONT := System.Null_Address;
      Top         : Integer := 0;
      Left        : Integer := 0;
      Height      : Integer := 0;
      Width       : Integer := 0;
      WndProc     : Win32_LONG := 0;
    end record;

  procedure Cleanup (Object : in out Window_Internals);

  ----------------------------------------------------------------------------
  --
  --                C O N T A I N E R _ I N T E R N A L S
  --
  --  This is a type derived from Window_Internals for use by container
  --  windows. It includes the following additional component:
  --
  --  Group : a flag to determine whether the WS_GROUP style should be
  --          applied to child controls (used to ensure that radiobutton
  --          groups are correctly delimited).
  --
  ----------------------------------------------------------------------------

  type Container_Internals is new Window_Internals with
    record
      Group : Boolean := True;
    end record;

  ----------------------------------------------------------------------------
  --
  --              M A I N _ W I N D O W _ I N T E R N A L S
  --
  --  This is a type derived from Container_Internals for use by top-level
  --  windows. It includes the following additional component:
  --
  --  Focus : the handle of the child window to activate when the top-level
  --          window is activated (if any).
  --
  ----------------------------------------------------------------------------

  type Main_Window_Internals is new Container_Internals with
    record
      Focus : Win32_HWND := System.Null_Address;
    end record;

  type Main_Window_Ptr is access all Main_Window_Internals;

  procedure Cleanup (Object : in out Main_Window_Internals);

  ----------------------------------------------------------------------------
  --
  --                   C A N V A S _ I N T E R N A L S
  --
  --  This is a type derived from Window_Internals for use by canvas
  --  window. It includes the following additional component:
  --
  --  Monitor : a protected record (defined in Canvas_Implementation)
  --            to record the drawing operations and mouse state and
  --            to synchronise accesses from the message loop task.
  --
  ----------------------------------------------------------------------------

  type Canvas_Internals is new Window_Internals with
    record
      Monitor  : Canvas_Monitor;
      Keypress : Integer := -1;
    end record;

  procedure Cleanup (Object : in out Canvas_Internals);

  type Canvas_Ptr is access all Canvas_Internals;

  ----------------------------------------------------------------------------
  --
  --            C O M M O N _ D I A L O G _ I N T E R N A L S
  --
  --  Common_Dialog_Internals (or a type derived from it) is what the
  --  Controlled_Type object in every Common_Dialog_Type object actually
  --  points to.
  --
  ----------------------------------------------------------------------------

  type Common_Dialog_Internals is
                     abstract new Reference_Counted_Type with null record;
  type Common_Dialog_Ptr is access all Common_Dialog_Internals'Class;
  function Show_Dialog (Dialog : access Common_Dialog_Internals)
                                               return Boolean is abstract;

  ----------------------------------------------------------------------------
  --
  --            C O L O U R _ D I A L O G _ I N T E R N A L S
  --
  ----------------------------------------------------------------------------

  type Custom_Colours is array (1..16) of aliased Win32_COLORREF;

  type Colour_Dialog_Internals is new Common_Dialog_Internals with
    record
      Struct : aliased Win32_CHOOSECOLOR;
      Colour : Colour_Type;
      Custom : Custom_Colours;
    end record;

  type Colour_Dialog_Ptr is access all Colour_Dialog_Internals'Class;

  function Show_Dialog (Dialog : access Colour_Dialog_Internals)
                                                      return Boolean;

  ----------------------------------------------------------------------------
  --
  --              F O N T _ D I A L O G _ I N T E R N A L S
  --
  ----------------------------------------------------------------------------

  type Font_Dialog_Internals is new Common_Dialog_Internals with
    record
      Struct : aliased Win32_CHOOSEFONT;
      Font   : aliased Win32_LOGFONT;
    end record;

  type Font_Dialog_Ptr is access all Font_Dialog_Internals'Class;

  function Show_Dialog (Dialog : access Font_Dialog_Internals)
                                                    return Boolean;

  ----------------------------------------------------------------------------
  --
  --              F I L E _ D I A L O G _ I N T E R N A L S
  --
  ----------------------------------------------------------------------------

  type File_Dialog_Internals (N : Win32_SIZE) is
                                  abstract new Common_Dialog_Internals with
    record
      Struct    : aliased Win32_OPENFILENAME;
      Title     : Win32_String (1..N);
      Buffer    : Win32_String (1..300);
      Directory : aliased Win32_String (1..300);
      Filter    : aliased Win32_String (1..300);
      Length    : Win32_SIZE := 0;
    end record;

  type File_Dialog_Ptr is access all File_Dialog_Internals'Class;
  
  ----------------------------------------------------------------------------
  --
  --              O P E N _ D I A L O G _ I N T E R N A L S
  --
  ----------------------------------------------------------------------------

  type Open_Dialog_Internals is new File_Dialog_Internals with null record;

  function Show_Dialog (Dialog : access Open_Dialog_Internals)
                                                    return Boolean;

  ----------------------------------------------------------------------------
  --
  --              S A V E _ D I A L O G _ I N T E R N A L S
  --
  ----------------------------------------------------------------------------

  type Save_Dialog_Internals is new File_Dialog_Internals with null record;

  function Show_Dialog (Dialog : access Save_Dialog_Internals)
                                                    return Boolean;

end JEWL.Window_Implementation;
------------------------------------------------------------------------------
--                                                                          --
--                                 J E W L                                  --
--                                                                          --
--   Body of the top-level package providing I/O and Windows facilities     --
--   for beginners.                                                         --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl.adb 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl.adb $
-- Revision 1.7  2007/01/08 17:00:00  JE
-- * Fixed linker options in JEWL.Win32_Interface to accommodate changes to GNAT
--   GPL 2006 compiler (thanks to John McCormick for this)
-- * Added delay in message loop to avoid the appearance of hogging 100% of CPU
--   time
--
-- Revision 1.6  2001/11/02 16:00:00  JE
-- * Fixed canvas bug when saving an empty canvas
-- * Restore with no prior save now acts as erase
-- * Removed redundant variable declaration in Image function
--
-- Revision 1.5  2001/08/22 15:00:00  JE
-- * Minor bugfix to Get_Text for combo boxes
-- * Minor changes to documentation (including new example involving dialogs)
--
-- Revision 1.4  2001/01/25 09:00:00  je
-- Changes visible to the user:
--
-- * Added support for drawing bitmaps on canvases (Draw_Image operations
--   and new type Image_Type)
-- * Added Play_Sound
-- * Added several new operations on all windows: Get_Origin, Get_Width,
--   Get_Height, Set_Origin, Set_Size and Focus
-- * Added several functions giving screen and window dimensions: Screen_Width,
--   Screen_Height, Frame_Width, Frame_Height, Dialog_Width, Dialog_Height and
--   Menu_Height
-- * Canvases can now handle keyboard events: new constructor and Key_Code added
-- * Added procedure Play_Sound
-- * Operations "+" and "-" added for Point_Type
-- * Pens can now be zero pixels wide
-- * The absolute origin of a frame can now have be specified when the frame
--   is created
-- * Added new File_Dialog operations Add_Filter and Set_Directory
-- * Added Get_Line renames to JEWL.IO for compatibility with Ada.Text_IO
-- * Added all the Get(File,Item) operations mentioned in documentation but
--   unaccountably missing :-(
-- * Documentation updated to reflect the above changes
-- * HTML versions of public package specifications added with links from
--   main documentation pages
--
-- Other internal changes:
--
-- * Canvas fonts, pens etc. now use JEWL.Reference_Counted_Type rather than
--   reinventing this particular wheel, as do images
-- * Various minor code formatting changes: some code reordered for clarity,
--   some comments added or amended,
-- * Changes introduced in 1.2 to support GNAT 3.10 have been reversed, since
--   GNAT 3.10 still couldn't compile this code correctly... ;-(
--
-- Outstanding issues:
--
-- * Optimisation breaks the code (workaround: don't optimise)
--
-- Revision 1.3  2000/07/07 12:00:00  je
-- * JEWL.Simple_Windows added; JEWL.IO modified to use JEWL.Simple_Windows.
-- * JEWL.IO bug fix: Put_Line to file wrote newline to standard output
--   instead of to the file (thanks to Jeff Carter for pointing this out).
-- * Panels fixed so that mouse clicks are passed on correctly to subwindows.
-- * Memos fixed so that tabs are handled properly.
-- * Password feature added to editboxes.
-- * Minor typos fixed in comments within the package sources.
-- * Documentation corrected and updated following comments from Moti Ben-Ari
--   and Don Overheu.
--
-- Revision 1.2  2000/04/18 20:00:00  je
-- * Minor code changes to enable compilation by GNAT 3.10
-- * Minor documentation errors corrected
-- * Some redundant "with" clauses removed
--
-- Revision 1.1  2000/04/18 20:00:00  je
-- Initial revision
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body JEWL is

  ----------------------------------------------------------------------------
  --
  --        O P E R A T I O N S   O N   S U P P O R T   T Y P E S
  --
  ----------------------------------------------------------------------------
  --
  --  Light: generate a lightened version of a given colour by increasing the
  --         intensity of each hue by 50%.
  --
  function Light (Colour : Colour_Type) return Colour_Type is
  begin
    return (Red   => (Colour.Red+Colour_Range'Last+1)/2,
            Green => (Colour.Green+Colour_Range'Last+1)/2,
            Blue  => (Colour.Blue+Colour_Range'Last+1)/2);
  end Light;

  ----------------------------------------------------------------------------
  --
  --  Dark: generate a darkened version of a given colour by decreasing the
  --        intensity of each hue by 50%.
  --
  function Dark (Colour : Colour_Type) return Colour_Type is
  begin
    return (Red   => Colour.Red/2,
            Green => Colour.Green/2,
            Blue  => Colour.Blue/2);
  end Dark;

  ----------------------------------------------------------------------------
  --
  --  Font: create a Font_Type structure which has the length of the font
  --        name as a discriminant.
  --
  function Font  (Name   : String;
                  Size   : Positive;
                  Bold   : Boolean  := False;
                  Italic : Boolean  := False) return Font_Type is
    F : Font_Type(Name'Length);
  begin
    F.Name   := Name;
    F.Size   := Size;
    F.Bold   := Bold;
    F.Italic := Italic;
    return F;
  end Font;

  ----------------------------------------------------------------------------
  --
  --  Name: get the name of a font's typeface.
  --
  function Name (Font : Font_Type) return String is
  begin
    return Font.Name;
  end Name;

  ----------------------------------------------------------------------------
  --
  --  Size: get the size of a font in points.
  --
  function Size (Font : Font_Type) return Natural is
  begin
    return Font.Size;
  end Size;

  ----------------------------------------------------------------------------
  --
  --  Bold: True is the specified font is bold.
  --
  function Bold (Font : Font_Type) return Boolean is
  begin
    return Font.Bold;
  end Bold;

  ----------------------------------------------------------------------------
  --
  --  Italic: True is the specified font is italic.
  --
  function Italic (Font : Font_Type) return Boolean is
  begin
    return Font.Italic;
  end Italic;

  ----------------------------------------------------------------------------
  --
  --  Endpoint: calculate the endpoint of a line drawn from a specified origin
  --            for a given length at a given angle.
  --
  function Endpoint (From   : Point_Type;
                     Length : Positive;
                     Angle  : Angle_Type) return Point_Type is
  begin
    return (From.X + Integer(Float(Length)*Sin(Float(Angle),360.0)),
            From.Y - Integer(Float(Length)*Cos(Float(Angle),360.0)));
  end Endpoint;

  ----------------------------------------------------------------------------
  --
  --  Inside: test if Point is inside the rectangle defined by From and To,
  --          bearing in mind that any two opposite corners can be given
  --          (not necessarily top left and bottom right).
  --
  function Inside (Point : Point_Type;
                   From  : Point_Type;
                   To    : Point_Type) return Boolean is
  begin
    return Point.X >= Integer'Min(From.X,To.X) and
           Point.X <= Integer'Max(From.X,To.X) and
           Point.Y >= Integer'Min(From.Y,To.Y) and
           Point.Y <= Integer'Max(From.Y,To.Y);
  end Inside;

  ----------------------------------------------------------------------------
  --
  --  "+": add two points (P1.X + P2.X, P1.Y + P2.Y).
  --
  function "+" (P1, P2 : Point_Type) return Point_Type is
  begin
    return (X => P1.X+P2.X, Y => P1.Y+P2.Y);
  end "+";

  ----------------------------------------------------------------------------
  --
  --  "-": subtract two points (P1.X - P2.X, P1.Y - P2.Y).
  --
  function "-" (P1, P2 : Point_Type) return Point_Type is
  begin
    return (X => P1.X-P2.X, Y => P1.Y-P2.Y);
  end "-";

  ----------------------------------------------------------------------------
  --
  --                I N T E R N A L   O P E R A T I O N S
  --
  ----------------------------------------------------------------------------
  --
  --  Free: deallocate a reference-counted object.
  --
  procedure Free is new Ada.Unchecked_Deallocation
                                (Reference_Counted_Type'Class,
                                 Reference_Counted_Ptr);

  ----------------------------------------------------------------------------
  --
  --  Cleanup: the finalisation primitive for reference-counted types.
  --           Override this for derived types to do something useful.
  --
  procedure Cleanup (Object : in out Reference_Counted_Type) is
  begin
    null;
  end Cleanup;

  ----------------------------------------------------------------------------
  --
  --  Finalize: decrement the reference count when an object containing
  --            a pointer to a reference-counted object is destroyed.
  --            When the reference count reaches zero, finalise the
  --            reference-counted object and free its memory.
  --
  procedure Finalize (Object : in out Controlled_Type) is
  begin
    if Object.Pointer /= null then
      if Object.Pointer.Count > 0 then
        Object.Pointer.Count := Object.Pointer.Count - 1;
        if Object.Pointer.Count = 0 then
          Cleanup (Object.Pointer.all);
          Free (Object.Pointer);
        end if;
      end if;
    end if;
  end Finalize;

  ----------------------------------------------------------------------------
  --  Adjust: bump the reference count when copying an object containing a
  --          pointer to a reference-counted object. Do nothing if the
  --          pointer is null.
  --
  procedure Adjust (Object : in out Controlled_Type) is
  begin
    if Object.Pointer /= null then
      Object.Pointer.Count := Object.Pointer.Count + 1;
    end if;
  end Adjust;

end JEWL;
------------------------------------------------------------------------------
--                                                                          --
--                                 J E W L                                  --
--                                                                          --
--   Top-level package in a hierarchy providing I/O and Windows facilities  --
--   for beginners.                                                         --
--                                                                          --
--   Copyright (C) John English 2000. Contact address: je@brighton.ac.uk    --
--   This software is released under the terms of the GNU General Public    --
--   License and is intended primarily for educational use. Please contact  --
--   the author to report bugs, suggestions and modifications.              --
--                                                                          --
------------------------------------------------------------------------------
-- $Id: jewl.ads 1.7 2007/01/08 17:00:00 JE Exp $
------------------------------------------------------------------------------
--
-- $Log: jewl.ads $
-- Revision 1.7  2007/01/08 17:00:00  JE
-- * Fixed linker options in JEWL.Win32_Interface to accommodate changes to GNAT
--   GPL 2006 compiler (thanks to John McCormick for this)
-- * Added delay in message loop to avoid the appearance of hogging 100% of CPU
--   time
--
-- Revision 1.6  2001/11/02 16:00:00  JE
-- * Fixed canvas bug when saving an empty canvas
-- * Restore with no prior save now acts as erase
-- * Removed redundant variable declaration in Image function
--
-- Revision 1.5  2001/08/22 15:00:00  JE
-- * Minor bugfix to Get_Text for combo boxes
-- * Minor changes to documentation (including new example involving dialogs)
--
-- Revision 1.4  2001/01/25 09:00:00  je
-- Changes visible to the user:
--
-- * Added support for drawing bitmaps on canvases (Draw_Image operations
--   and new type Image_Type)
-- * Added Play_Sound
-- * Added several new operations on all windows: Get_Origin, Get_Width,
--   Get_Height, Set_Origin, Set_Size and Focus
-- * Added several functions giving screen and window dimensions: Screen_Width,
--   Screen_Height, Frame_Width, Frame_Height, Dialog_Width, Dialog_Height and
--   Menu_Height
-- * Canvases can now handle keyboard events: new constructor and Key_Code added
-- * Added procedure Play_Sound
-- * Operations "+" and "-" added for Point_Type
-- * Pens can now be zero pixels wide
-- * The absolute origin of a frame can now have be specified when the frame
--   is created
-- * Added new File_Dialog operations Add_Filter and Set_Directory
-- * Added Get_Line renames to JEWL.IO for compatibility with Ada.Text_IO
-- * Added all the Get(File,Item) operations mentioned in documentation but
--   unaccountably missing :-(
-- * Documentation updated to reflect the above changes
-- * HTML versions of public package specifications added with links from
--   main documentation pages
--
-- Other internal changes:
--
-- * Canvas fonts, pens etc. now use JEWL.Reference_Counted_Type rather than
--   reinventing this particular wheel, as do images
-- * Various minor code formatting changes: some code reordered for clarity,
--   some comments added or amended,
-- * Changes introduced in 1.2 to support GNAT 3.10 have been reversed, since
--   GNAT 3.10 still couldn't compile this code correctly... ;-(
--
-- Outstanding issues:
--
-- * Optimisation breaks the code (workaround: don't optimise)
--
-- Revision 1.3  2000/07/07 12:00:00  je
-- * JEWL.Simple_Windows added; JEWL.IO modified to use JEWL.Simple_Windows.
-- * JEWL.IO bug fix: Put_Line to file wrote newline to standard output
--   instead of to the file (thanks to Jeff Carter for pointing this out).
-- * Panels fixed so that mouse clicks are passed on correctly to subwindows.
-- * Memos fixed so that tabs are handled properly.
-- * Password feature added to editboxes.
-- * Minor typos fixed in comments within the package sources.
-- * Documentation corrected and updated following comments from Moti Ben-Ari
--   and Don Overheu.
--
-- Revision 1.2  2000/04/18 20:00:00  je
-- * Minor code changes to enable compilation by GNAT 3.10
-- * Minor documentation errors corrected
-- * Some redundant "with" clauses removed
--
-- Revision 1.1  2000/04/18 19:34:15  je
-- Initial revision
--
-- Revision 1.1  2000/04/09 21:00:00  je
-- Initial revision
--
------------------------------------------------------------------------------

with Ada.Finalization;

package JEWL is

  ----------------------------------------------------------------------------
  --
  --                      S U P P O R T   T Y P E S
  --
  --  These types are used elsewhere throughout this package library:
  --
  --  Angle_Type      : an angle specified as an integral number of
  --                    degrees (0 to 359)
  --  Colour_Type     : a colour specified as an RGB value.
  --  Font_Type       : a font specified by a name, point size and style
  --                    options.
  --  Point_Type      : a pair of (X,Y) coordinates within a window.
  --  Point_List      : an array of (X,Y) coordinate pairs.
  --
  ----------------------------------------------------------------------------

  type    Angle_Type     is mod 360;
  subtype Colour_Range   is Integer range 0..255;
  type    Colour_Type    is record
                              Red    : Colour_Range;
                              Green  : Colour_Range;
                              Blue   : Colour_Range;
                            end record;
  type    Font_Type (Length : Natural)
                         is record
                              Name   : String (1..Length);
                              Size   : Positive;
                              Bold   : Boolean := False;
                              Italic : Boolean := False;
                            end record;

  type    Point_Type     is record
                              X,Y : Integer;
                            end record;
  type    Point_List     is array (Positive range <>) of Point_Type;

  ----------------------------------------------------------------------------
  --
  --        O P E R A T I O N S   O N   S U P P O R T   T Y P E S
  --
  --  Colour operations:
  --    Light    : Generate a lightened version of a colour, e.g. Light(Red).
  --    Dark     : Generate a darkened version of a colour, e.g. Dark(Green).
  --
  --  Font operations:
  --    Font     : Generate a font with the specified properties.
  --    Name     : Get the name of the font typeface.
  --    Size     : Get the font size in points.
  --    Bold     : Test if the font is bold.
  --    Italic   : Test if the font is italic.
  --
  --  Point operations:
  --    Endpoint : Calculate the endpoint of a line from a starting point,
  --               length and angle.
  --    Inside   : Test if a specified point is inside a specified rectangle
  --               (defined by the coordinates of two diagonally opposite
  --               corners).
  --    P1 + P2  : Add two points (P1.X+P2.X, P1.Y+P2.Y).
  --    P1 - P2  : Subtract two points (P1.X-P2.X, P1.Y-P2.Y).
  --
  ----------------------------------------------------------------------------

  function Light    (Colour : Colour_Type) return Colour_Type;
  function Dark     (Colour : Colour_Type) return Colour_Type;

  function Font     (Name   : String;
                     Size   : Positive;
                     Bold   : Boolean  := False;
                     Italic : Boolean  := False)
                                           return Font_Type;
  function Name     (Font   : Font_Type)   return String;
  function Size     (Font   : Font_Type)   return Natural;
  function Bold     (Font   : Font_Type)   return Boolean;
  function Italic   (Font   : Font_Type)   return Boolean;

  function Endpoint (From   : Point_Type;
                     Length : Positive;
                     Angle  : Angle_Type)  return Point_Type;
  function Inside   (Point  : Point_Type;
                     From   : Point_Type;
                     To     : Point_Type)  return Boolean;
  function "+"      (P1, P2 : Point_Type)  return Point_Type;
  function "-"      (P1, P2 : Point_Type)  return Point_Type;

  ----------------------------------------------------------------------------
  --  Renamings for our transatlantic cousins, in the hope that some day
  --  they'll repay the favour/favor... ;-)
  ----------------------------------------------------------------------------

  subtype Color_Range is Colour_Range;
  subtype Color_Type  is Colour_Type;

private

  ----------------------------------------------------------------------------
  --  The private part of this package also provides a convenient place to
  --  declare the reference-counted types used by the generic package
  --  JEWL.Windows. Although they could be declared in the generic
  --  package JEWL.Windows, the use of a controlled type in a generic
  --  package would make it necessary to make all instantiations of the
  --  package at library level, a restriction which would probably baffle
  --  those for whom this software is intended.
  ----------------------------------------------------------------------------
  --
  --  A type which includes a reference count:
  --
  type Reference_Counted_Type is tagged limited
    record
      Count : Natural := 1;
    end record;

  ----------------------------------------------------------------------------
  --
  --  A primitive operation called when the last reference is deleted:
  --
  procedure Cleanup (Object : in out Reference_Counted_Type);

  ----------------------------------------------------------------------------
  --
  --  A pointer to the class of reference-counted types:
  --
  type Reference_Counted_Ptr is access all Reference_Counted_Type'Class;

  ----------------------------------------------------------------------------
  --
  --  A record which contains a pointer to a reference-counted object:
  --
  type Controlled_Type is new Ada.Finalization.Controlled with
    record
      Pointer : Reference_Counted_Ptr;
    end record;

  ----------------------------------------------------------------------------
  --
  --  Controlled operations:
  --
  procedure Finalize (Object : in out Controlled_Type);
  procedure Adjust   (Object : in out Controlled_Type);

end JEWL;
