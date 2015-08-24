# Introduction

This article is intended for developers. It is to list ways of fighting flicker in VCL applications, as applied to Wakan.

Compare Issue #116.

# What causes flicker

Flicker happens when something is drawn on the screen in several passes - for example, when the control is first filled with background color and then data is drawn over it.

Windows actually outputs all operations instantly, so there's a moment when the control is filled with background color but empty. This is what we perceive as flicker.

For there to be no flicker, each pixel must be colored no more than once, with it's final color.

Flicker is more noticeable when working in older OS (<=XP) or simplified themes (Desktop composition disabled) because Desktop composition performs some double-buffering internally.

## Double-buffering
To only paint once double-buffering is sometimes used. With double-buffering you draw in memory and then copy the resulting picture to the screen, pixel by pixel.

Delphi TWinControl descendants have standard double-buffering support (property DoubleBuffered) but it's not recommended to enable it except in certain cases:

  * It wastes a lot of memory
  * Many standard controls (ex. TScrollBar) are buggy when placed over DoubleBuffered control.

In any case, DoubleBuffered needs to be enabled only selectively, for precisely those controls which benefit from it.

Notably, any controls which are fully painted by yourself can benefit from double-buffering, allowing you to not reimplement it manually.

## WM\_ERASEBKGND and WM\_PAINT
Windows sends two types of paint messages to controls: first WM\_ERASEBKGND and then WM\_PAINT.

Most of the time, you don't want to do anything in WM\_ERASEBKGND. You should just return true and do all the erasing and painting in WM\_PAINT.

Controls which actually erase something in WM\_ERASEBKGND are a major source of flicker. When implementing your own control, explicitly handle WM\_ERASEBKGND and make it do absolutely nothing.

## Paint clipping
Some controls, such as TPanel, need to fill itself with background color, but not where other controls cover them (or there'd be flicker).

For that reason, Windows supports "clip area" which is a combination of rectangles defining where the drawing operations take effect, similar to selected area in Paint.

Before calling WM\_ERASEBKGND or WM\_PAINT, Windows sets clip area so that child controls are excluded.

# Major sources of flicker

## TGraphicControl descendants
They don't have their own Handle and WM\_ERASEBKGND/WM\_PAINT cycle and rely on parent for painting.
Therefore they have no choice but to first be erased with the parent (as Windows doesn't clip them out of the parent when calling its ERASEBKGND) and then redrawn in their Paint procedure.

Solutions:

  * Replace with TWinControl-descending native controls which are better in almost every aspect.
  * Set parent TWinControl's DoubleBuffered property to true.
  * Use ERASEBKGND-ignoring TWinControl as a parent.
  * Use personal DoubleBuffered non-FullRepaint TPanel for every control?

Infringing controls:

  * TSpeedButton (replace with TButton / TToolbar with one or more buttons)
  * TBevel (replace with TPanel where possible)
  * TShape (replace with custom TWinControl descendants which draw the shape and what's inside it)

## Custom-drawn controls
Custom-drawn controls are often drawn outside of a paint cycle. So standard DoubleBuffering property will not work.
Even if custom double-buffering is used, custom-drawn controls often do other drawing in Paint()  which is not cooperated with custom double-buffering and causes flicker.

Solutions:

  * Write proper TWinControl-descending control which implements required painting.
  * Use a properly tailored TWinControl-descending paintbox which does not handle ERASEBKGND and only paint in its OnPaint handler. To repaint it at any time, Invalidate() it.

## TPanel with FullRepaint set
By default TPanel has FullRepaint set which means that even though Windows clips out children controls and unchanged parts when calling its WM\_PAINT, TPanel still repaints all the client area.

In most cases it's better to turn it off so that only the area which was changed is repainted.

Solutions:

  * Disable FullRepaint for every panel which suffers from this.

## TGrid with DefaultDrawing enabled
TGrid descendants in Wakan usually draw all the contents in OnDrawCell. Such Grids are recommended to have DefaultDrawing disabled, otherwise for a split second they'll draw internal (ugly) representation of the cell every repaint, causing flicker.

Solutions:

  * Disable DefaultDrawing for owner-drawn TGrids and make sure all cells are properly drawn manually.


# Excessive repainting

Even if you repaint the control correctly, it may flicker if you do it several times, e.g. when adding a bunch of rows or changing multiple properties one by one.

Ideally you'd want to repaint the control only once, after all the changes have been made.

There are several steps to achieve that:

  * Do not "paint on reload". Paint the control in its `OnPaint` or `WM_PAINT` handler
  * Call `Invalidate` instead of `Repaint`
  * Disable updates when adding a lot of rows

## Do not paint on reload

Sometimes people repaint the control right in the code which populates it. This is especially common with user-drawn controls.

This is a bad approach, it should never be used. Windows may ask your control or parts of it to be repainted at any time. You should be prepared to respond to such requests, instead of only repainting at a limited number of occasions.

To repaint at any time when requested, write an `OnPaint` handler, override `Paint` procedure or add `WM_PAINT` message handler if you're writing a custom control.

At any occasion you think a forced repaint is needed, instead of manual repainting call `Invalidate`. This tells the OS to ask your control to be repainted in the standard fashion.


## Call `Invalidate` instead of `Repaint`

`Invalidate` marks the control as "dirty" but does not immediately repaint it. If you call `Invalidate` several times, it's still going to be repainted only once, after you finish processing.

When calling `Invalidate`, you may specify which part of the control needs repainting. When repainting, the system will clip out all parts of the control which weren't invalidated since the last repaint (faster + less flicker).


## Disable updates

When you add or remove a bunch of rows at the same time, every time you add one row, the control realigns and (with some controls) repaints itself which is slow and manifests as a flicker.

To combat that, disable updates, make all necessary changes and then only realign/repaint once.

Common ways of disabling updates include:

1. `BeginUpdate/EndUpdate` for the controls which support it. Not all controls have such methods.

2. `WM_SETREDRAW`. This works for all windows, but only disables the repainting. The control may continue to waste time on realign:

```

if Self.Visible then
SendMessage(Handle, WM_SETREDRAW, WPARAM(False), 0);

//...Make changes

if Self.Visible then begin
SendMessage(Handle, WM_SETREDRAW, WPARAM(True), 0);
RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
//Normal Invalidate is not enough
end;
```

3. `DisableAlign/EnableAlign`.