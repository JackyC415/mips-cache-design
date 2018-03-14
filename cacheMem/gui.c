#include "tips.h"

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <glib.h>

/*****************************************************************************
  GUI related structs, variables, and macros
 ****************************************************************************/

#ifdef CYGWIN

#define WINDOW_DEFAULT_WIDTH 800
#define WINDOW_DEFAULT_HEIGHT 600

#else

#define WINDOW_DEFAULT_WIDTH 900
#define WINDOW_DEFAULT_HEIGHT 600

#endif

#ifdef LINUX
#define GLOBAL_FONT_SIZE 8
#else
#define GLOBAL_FONT_SIZE 10
#endif

typedef enum {HIGHLIGHT_BLOCK, HIGHLIGHT_OFFSET} node_type;
typedef struct _node
{
  GdkColor* fg_color;
  GdkColor* bg_color;
  node_type type;
  gint unit_index;
  gint block_index;
  gint block_offset;
  gint x;
  gint y;
  gint width;
  gint height;
  struct _node* next;
} node;

node* drawlist;

/* Main GUI Panels */
GtkWidget* main_window;
GtkWidget* cache_canvas;
GtkWidget* register_canvas;
GtkWidget* textbox_scroll_window;
GtkWidget* textbox;
GtkTextMark* mark;

/* Configure dialog related variables */
GtkWidget* assoc_entry;
GtkWidget* index_entry;
GtkWidget* block_entry;
GtkWidget* random_policy_button;
GtkWidget* lru_policy_button;
GtkWidget* lfu_policy_button;
ReplacementPolicy panel_replacement_policy;
GtkWidget* write_back_policy_button;
GtkWidget* write_through_policy_button;
MemorySyncPolicy panel_memory_sync_policy;
GtkWidget* index_view_button;
GtkWidget* assoc_view_button;
CacheView panel_cache_view;

/* Run Dialog related variables */
GtkWidget* speed_slider;
guint timer_id;            /* used for cache automation test */
gboolean timer_active;

/* Color */
GdkColormap* cmap; 
GdkColor red;
GdkColor skyblue;
GdkColor white;
GdkColor black;
GdkColor palegreen;

/* Cache Display related variables */
gboolean base_display_variables_initialized;
gint base_x_offset;
gint base_y_offset;

gint char_width;
gint tab_width;
gint byte_width;
gint line_height;

gint cache_header_height;
gint block_header_width;
gint block_data_width;
gint cache_unit_height;

gint horizontal_line_width;

PangoLayout* layout;
PangoFontDescription* fontdesc;
PangoFontMetrics* metrics;

gchar* cache_header_text;
gchar* block_header_text;

/******************************************************************************
   Register display related functions
 *****************************************************************************/

gboolean draw_registers(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  static gchar* register_display[] = { 
    "R00: %08x\tR08: %08x\tR16: %08x\tR24: %08x",
    "R01: %08x\tR09: %08x\tR17: %08x\tR25: %08x",
    "R02: %08x\tR10: %08x\tR18: %08x\tR26: %08x",
    "R03: %08x\tR11: %08x\tR19: %08x\tR27: %08x",
    "R04: %08x\tR12: %08x\tR20: %08x\tR28: %08x",
    "R05: %08x\tR13: %08x\tR21: %08x\tR29: %08x",
    "R06: %08x\tR14: %08x\tR22: %08x\tR30: %08x",
    "R07: %08x\tR15: %08x\tR23: %08x\tR31: %08x",
    " PC: %08x"
  };

  gint i;
  gchar buffer[200];
  gsize buffer_size;

#ifdef CYGWIN
  static PangoLayout* pango = NULL;
#else
  PangoLayout* pango;
#endif
  PangoFontDescription* font_desc;
  
  sprintf(buffer, "Monospace %d", GLOBAL_FONT_SIZE);
  font_desc = pango_font_description_from_string(buffer);
#ifdef CYGWIN
  if(pango == NULL)
#endif
    pango = gtk_widget_create_pango_layout(widget, NULL);
  pango_layout_set_font_description(pango, font_desc);   

  /* Display registers */
  for(i = 0; i < 8; i++)
  {
    buffer_size = sprintf(buffer, register_display[i], registers[i], registers[i + 8], registers[i + 16], registers[i+24]);

    pango_layout_set_text(pango, buffer, buffer_size);

    gdk_draw_layout(widget->window, 
		    widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		    10, i * 15,
		    pango);
  }

  /* Display PC */
  buffer_size = sprintf(buffer, register_display[i], PC);
  pango_layout_set_text(pango, buffer, buffer_size);
  gdk_draw_layout(widget->window, 
		  widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		  10, i * 15,
		  pango);

#ifndef LINUX
#ifndef CYGWIN
  g_free(pango);
#endif
  g_free(font_desc);
#endif

  return TRUE;
}

void refresh_register_display()
{
  if(IS_GUI_ACTIVE())
    gtk_widget_queue_draw(register_canvas);
}

GtkWidget* build_register_panel()
{
  GtkWidget* frame;

  /*
#ifdef CYGWIN
  register_canvas = gtk_text_view_new();
  gtk_text_view_set_editable(GTK_TEXT_VIEW(register_canvas), FALSE);
#else

#endif
  */

  register_canvas = gtk_drawing_area_new ();
  g_signal_connect (G_OBJECT (register_canvas), "expose_event", G_CALLBACK (draw_registers), NULL);

  frame = gtk_frame_new("Register Contents");
  gtk_container_add(GTK_CONTAINER(frame), register_canvas);

  return frame;
}

/******************************************************************************
   Log Panel related functions
 *****************************************************************************/

gboolean log_event(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  static int counter = 0;
  gchar zbuffer[4];

  GtkTextBuffer* buffer;
  GtkTextIter iter;
  
  buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(widget));
  gtk_text_buffer_get_end_iter(buffer, &iter);
  gtk_text_buffer_place_cursor(buffer, &iter);

  /* Inserts new lines and scroll to new insertion */
  sprintf(zbuffer, "[%d]\n", counter++);
  gtk_text_buffer_insert_at_cursor(buffer, zbuffer, -1);
  gtk_text_buffer_get_end_iter(buffer, &iter);
  gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(widget), &iter, 0, FALSE, 0, 0);

  return TRUE;
}

void append_log(char* msg)
{
  GtkTextBuffer* buffer;
  GtkTextIter iter;

  if(!(IS_GUI_ACTIVE()))
  {
     printf(msg);
     return;
  }

  /* Gets buffer */
  buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textbox));
  gtk_text_buffer_get_end_iter(buffer, &iter);

  /* Inserts msg and scrolls to new insertion */
  gtk_text_buffer_insert(buffer, &iter, msg, -1);
  gtk_text_view_scroll_to_mark(GTK_TEXT_VIEW(textbox), mark, 0, FALSE, 0, 0);
}

GtkWidget* build_log_panel()
{
  GtkTextBuffer* buffer;
  GtkTextIter iter;
  GtkWidget* frame;
  GtkWidget* window;
  
  /* Build textbox */
  textbox = gtk_text_view_new();
  gtk_text_view_set_editable(GTK_TEXT_VIEW(textbox), FALSE);
  
  /* Initialize text in buffer */
  buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textbox));
  gtk_text_buffer_set_text(buffer, "TIPS v2 started\n", -1);
  gtk_text_buffer_get_end_iter(buffer, &iter);
  mark = gtk_text_buffer_create_mark(buffer, "end", &iter, FALSE);
  /*  g_signal_connect (G_OBJECT (textbox), "button_press_event", G_CALLBACK (log_event), NULL); */

  /* Place textbox into frame */
  textbox_scroll_window = window = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(window), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
  gtk_container_add(GTK_CONTAINER(window), textbox);

  /* Build frame around textbox */
  frame = gtk_frame_new("Execution Log");
  gtk_container_add(GTK_CONTAINER(frame), window);
  
  return frame;
}

/******************************************************************************
   Cache Display related functions
 *****************************************************************************/

void configure_cache_drawing_parameters(GtkWidget* widget)
{
  int offset;
  gchar buffer[200];
  gint buffer_size;

  /* free existing data */
  if(fontdesc != NULL)
    pango_font_description_free(fontdesc);

  if(layout != NULL)
    g_object_unref(layout);

  /* Init base offsets */
  base_x_offset = 15;
  base_y_offset = 15;

  /* Set layout to use the following font when drawing text */
  sprintf(buffer, "Monospace %d", GLOBAL_FONT_SIZE);
  fontdesc = pango_font_description_from_string(buffer);
  layout = gtk_widget_create_pango_layout(widget, NULL);
  pango_layout_set_font_description(layout, fontdesc);

  /* Init base text information */
  metrics = pango_context_get_metrics(gtk_widget_get_pango_context(widget), fontdesc, pango_context_get_language(gtk_widget_get_pango_context(widget)));
  char_width = PANGO_PIXELS(pango_font_metrics_get_approximate_char_width(metrics));
  line_height = PANGO_PIXELS(pango_font_metrics_get_ascent(metrics) + pango_font_metrics_get_descent(metrics));

  /* Init common widths and heights */
  tab_width = 8 * char_width;
  byte_width = 2 * char_width;  
  cache_unit_height = set_count * line_height;

  /* Init header size information */
  switch(view)
  {
  case ASSOC:
    cache_header_text = "  Unit #%d\n\n  Blk V D LRU\tLFU\tTag\n";
    break;
  case INDEX:
    cache_header_text = "  Set V D LRU\tLFU\tTag\n";
    break;
  default:
    printf("Invalid cache arrangement");
    exit(1);
  }
  buffer_size = sprintf(buffer, cache_header_text, MAX_ASSOC); 
  pango_layout_set_text(layout, buffer, buffer_size);
  pango_layout_get_pixel_size(layout, NULL, &cache_header_height);

  /* Init block header size information */
  block_header_text = "  %2d  %d %d %s\t%s\t%08X   ";
  buffer_size = sprintf(buffer, block_header_text, 0, cache[0].block[0].valid, cache[0].block[0].dirty, lru_to_string(0, 0), lfu_to_string(0,0), cache[0].block[0].tag);
  pango_layout_set_text(layout, buffer, buffer_size);
  pango_layout_get_pixel_size(layout, &block_header_width, NULL);

  /* Init block width information */
  block_data_width = 0;
  for(offset = 0; offset < block_size; offset++)
  {
    block_data_width += byte_width;

    if(((offset + 1) % 4) == 0)
      block_data_width += (3 * char_width);
    else if((offset + 1) % 2 == 0)
      block_data_width += char_width;
  }

  /* Init dividing line information */
  horizontal_line_width = block_header_width + block_data_width + byte_width;
}

gboolean draw_cache_display_index(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  gint s;
  gint b;
  gint o;

  gint x_offset;
  gint y_offset;

  gchar buffer[250];
  gsize buffer_size;

  node* current;

  /* Get information about height and width of characters */
  if(base_display_variables_initialized == FALSE)
  {
    configure_cache_drawing_parameters(widget);
    base_display_variables_initialized = TRUE;
  }

  /* Display message if any of the cache parameters are zero */
  if(assoc == 0 || set_count == 0 || block_size == 0)
  {
    PangoFontDescription* msg_fontdesc = pango_font_description_from_string("Monospace 14");
    pango_layout_set_font_description(layout, msg_fontdesc);

    buffer_size = sprintf(buffer, "\n\n\n\n\n\nNo cache exists;\nClick on 'Config Cache' to configure a cache");
    pango_layout_set_text(layout, buffer, buffer_size);
    gdk_draw_layout(widget->window, 
		    widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		    base_x_offset, 
		    base_y_offset,
		    layout);

    /* Restore font */
    pango_font_description_free(msg_fontdesc);
    pango_layout_set_font_description(layout, fontdesc);
    return TRUE;
  }

  x_offset = base_x_offset;
  y_offset = base_y_offset;
  
  /* Draw header */    
  buffer_size = sprintf(buffer, cache_header_text, s); 
  pango_layout_set_text(layout, buffer, buffer_size);
  gdk_draw_layout(widget->window, 
		  widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		  x_offset, 
		  y_offset,
		  layout);
  x_offset = base_x_offset;
  y_offset += cache_header_height;
    
  gdk_draw_line(widget->window,
		widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		base_x_offset,
		y_offset - (line_height / 2),
		horizontal_line_width,
		y_offset - (line_height / 2));

  for(b = 0; b < set_count; b++)
  {
    for(s = 0; s < assoc; s++)
    {
      buffer_size = sprintf(buffer, block_header_text, b, cache[b].block[s].valid, cache[b].block[s].dirty, lru_to_string(b, s), lfu_to_string(b, s), cache[b].block[s].tag);
      pango_layout_set_text(layout, buffer, buffer_size);
      gdk_draw_layout(widget->window, 
		      widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		      x_offset,
		      y_offset,
		      layout);      
      x_offset += block_header_width;

      for(o = 0; o < block_size; o++)
      {
	/* Print offset headers */
	if(b == 0 && s == 0 && ((o % 4) == 0))
	{
	  buffer_size = sprintf(buffer, "%02X", o);
	  pango_layout_set_text(layout, buffer, buffer_size);
	  gdk_draw_layout(widget->window,
			  widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
			  x_offset, 
			  y_offset - (2 * line_height),
			  layout);
	}

	buffer_size = sprintf(buffer, "%02X", cache[b].block[s].data[o]);
	pango_layout_set_text(layout, buffer, buffer_size);
	gdk_draw_layout(widget->window, 
			widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
			x_offset,
			y_offset,
			layout);
	x_offset += byte_width;

#ifdef CYGWIN
	if(((o + 1) % 4) == 0)
	  x_offset += (3 * char_width);
	else if((o + 1) % 2 == 0)
	  x_offset += char_width;
#else
	if(((o + 1) % 4) == 0)
	  x_offset += char_width;
#endif
      }
      y_offset += line_height;
      x_offset = base_x_offset;
    }

    /* Draw horizontal dividing line */
    gdk_draw_line(widget->window,
		  widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		  base_x_offset,
		  y_offset + (line_height / 2),
		  horizontal_line_width,
		  y_offset + (line_height / 2));
    y_offset += line_height;
    x_offset = base_x_offset;
  }

  /* Draw highlights first */
  current = drawlist;
  while(current != NULL)
  {
    switch(current->type)
    {
    case HIGHLIGHT_BLOCK:
      gdk_draw_rectangle(widget->window,
			 widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
			 FALSE,
			 current->x, current->y,
			 current->width, current->height);
      break;
    case HIGHLIGHT_OFFSET:
      x_offset = current->x;
      y_offset = current->y;
#ifndef CYGWIN
	/* Hack for Sparc/Solaris because there are gaps between adjacent bytes */
	buffer_size = sprintf(buffer, "        ");
	pango_layout_set_text(layout, buffer, buffer_size);
	gdk_draw_layout_with_colors(widget->window, 
				    widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
				    x_offset,
				    y_offset,
				    layout,
				    current->fg_color,
				    current->bg_color);	
#endif

      for(o = current->block_offset; o < current->block_offset + sizeof(instruction); o++)
      {
	buffer_size = sprintf(buffer, "%02X", cache[current->block_index].block[current->unit_index].data[o]);
	pango_layout_set_text(layout, buffer, buffer_size);
	gdk_draw_layout_with_colors(widget->window, 
				    widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
				    x_offset,
				    y_offset,
				    layout,
				    current->fg_color,
				    current->bg_color);
	x_offset += byte_width;

#ifdef CYGWIN
	if(((o + 1) % 2 == 0) && ((o + 1) % 4 != 0))
	{
	  buffer_size = sprintf(buffer, " ");
	  pango_layout_set_text(layout, buffer, buffer_size);
	  gdk_draw_layout_with_colors(widget->window, 
				      widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
				      x_offset,
				      y_offset,
				      layout,
				      current->fg_color,
				      current->bg_color);	  
	  x_offset += char_width;
	}
#endif
      }
      break;
    default:
      printf("Invalid type for highlight block\n");
      exit(1);      
    }
    current = current->next;
  }

  gtk_widget_set_size_request(widget, 400, y_offset);

  return TRUE;

}

gboolean draw_cache_display_assoc (GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  gint s;
  gint b;
  gint o;

  gint x_offset;
  gint y_offset;

  gchar buffer[250];
  gsize buffer_size;

  node* current;

  /* Get information about height and width of characters */
  if(base_display_variables_initialized == FALSE)
  {
    configure_cache_drawing_parameters(widget);
    base_display_variables_initialized = TRUE;
  }

  /* Display message if any of the cache parameters are zero */
  if(assoc == 0 || set_count == 0 || block_size == 0)
  {
    PangoFontDescription* msg_fontdesc = pango_font_description_from_string("Monospace 14");
    pango_layout_set_font_description(layout, msg_fontdesc);

    buffer_size = sprintf(buffer, "\n\n\n\n\n\nNo cache exists;\nClick on 'Config Cache' to configure a cache");
    pango_layout_set_text(layout, buffer, buffer_size);
    gdk_draw_layout(widget->window, 
		    widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		    base_x_offset, 
		    base_y_offset,
		    layout);

    /* Restore font */
    pango_font_description_free(msg_fontdesc);
    pango_layout_set_font_description(layout, fontdesc);
    return TRUE;
  }

  x_offset = base_x_offset;
  y_offset = base_y_offset;
  for(s = 0; s < assoc; s++)
  {
    /* Draw header */    
    buffer_size = sprintf(buffer, cache_header_text, s); 
    pango_layout_set_text(layout, buffer, buffer_size);
    gdk_draw_layout(widget->window, 
		    widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		    x_offset, 
		    y_offset,
		    layout);
    x_offset = base_x_offset;
    y_offset += cache_header_height;
    
    gdk_draw_line(widget->window,
		  widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		  base_x_offset,
		  y_offset - (line_height / 2),
		  horizontal_line_width,
		  y_offset - (line_height / 2));

    for(b = 0; b < set_count; b++)
    {      
      buffer_size = sprintf(buffer, block_header_text, b, cache[b].block[s].valid, cache[b].block[s].dirty, lru_to_string(b, s), lfu_to_string(b, s), cache[b].block[s].tag);
      pango_layout_set_text(layout, buffer, buffer_size);
      gdk_draw_layout(widget->window, 
		      widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		      x_offset,
		      y_offset,
		      layout);      
      x_offset += block_header_width;

      for(o = 0; o < block_size; o++)
      {
	/* Print offset headers */
	if(b == 0 && ((o % 4) == 0))
	{
	  buffer_size = sprintf(buffer, "%02X", o);
	  pango_layout_set_text(layout, buffer, buffer_size);
	  gdk_draw_layout(widget->window,
			  widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
			  x_offset, 
			  y_offset - (2 * line_height),
			  layout);
	}

	buffer_size = sprintf(buffer, "%02X", cache[b].block[s].data[o]);
	pango_layout_set_text(layout, buffer, buffer_size);
	gdk_draw_layout(widget->window, 
			widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
			x_offset,
			y_offset,
			layout);
	x_offset += byte_width;

#ifdef CYGWIN
	if(((o + 1) % 4) == 0)
	  x_offset += (3 * char_width);
	else if((o + 1) % 2 == 0)
	  x_offset += char_width;
#else
	if(((o + 1) % 4) == 0)
	  x_offset += char_width;
#endif
      }
      
      /* Draw horizontal dividing line */
      y_offset += line_height;
      if(((b+1) % 4) == 0)
      {
	gdk_draw_line(widget->window,
		      widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		      base_x_offset,
		      y_offset + (line_height / 2),
		      horizontal_line_width,
		      y_offset + (line_height / 2));

	y_offset += line_height;
      }
      x_offset = base_x_offset;
    }
    y_offset += line_height;
  }

  /* Draw highlights first */
  current = drawlist;
  while(current != NULL)
  {
    switch(current->type)
    {
    case HIGHLIGHT_BLOCK:
      gdk_draw_rectangle(widget->window,
			 widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
			 FALSE,
			 current->x, current->y,
			 current->width, current->height);
      break;
    case HIGHLIGHT_OFFSET:
      x_offset = current->x;
      y_offset = current->y;
#ifndef CYGWIN
	/* Hack for Sparc/Solaris because there are gaps between adjacent bytes */
	buffer_size = sprintf(buffer, "        ");
	pango_layout_set_text(layout, buffer, buffer_size);
	gdk_draw_layout_with_colors(widget->window, 
				    widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
				    x_offset,
				    y_offset,
				    layout,
				    current->fg_color,
				    current->bg_color);	
#endif

      for(o = current->block_offset; o < current->block_offset + sizeof(instruction); o++)
      {
	buffer_size = sprintf(buffer, "%02X", cache[current->block_index].block[current->unit_index].data[o]);
	pango_layout_set_text(layout, buffer, buffer_size);
	gdk_draw_layout_with_colors(widget->window, 
				    widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
				    x_offset,
				    y_offset,
				    layout,
				    current->fg_color,
				    current->bg_color);
	x_offset += byte_width;

#ifdef CYGWIN
	if(((o + 1) % 2 == 0) && ((o + 1) % 4 != 0))
	{
	  buffer_size = sprintf(buffer, " ");
	  pango_layout_set_text(layout, buffer, buffer_size);
	  gdk_draw_layout_with_colors(widget->window, 
				      widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
				      x_offset,
				      y_offset,
				      layout,
				      current->fg_color,
				      current->bg_color);	  
	  x_offset += char_width;
	}
#endif
      }
      break;
    default:
      printf("Invalid type for highlight block\n");
      exit(1);      
    }
    current = current->next;
  }

  gtk_widget_set_size_request(widget, 400, y_offset);

  return TRUE;

}

gboolean draw_cache_display (GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  switch(view)
  {
  case ASSOC:
    return draw_cache_display_assoc(widget, event, data);
  case INDEX:
    return draw_cache_display_index(widget, event, data);
  default:
    printf("Invalid arrangement of cache");
    exit(1);
  }

  return FALSE;
}

void refresh_cache_display()
{
  if(IS_GUI_ACTIVE())
    gtk_widget_queue_draw(cache_canvas);
}

void flush_drawlist()
{
  node* current;

  while(drawlist != NULL)
  {
    current = drawlist;
    drawlist = current->next;
    free(current);
  }
}

GtkWidget* build_drawing_panel()
{
  GtkWidget* window;

  /* Build cache display */
  cache_canvas = gtk_drawing_area_new ();
  /* gtk_widget_set_size_request(cache_canvas, 400, 1700); */
  gtk_widget_modify_bg(cache_canvas, GTK_STATE_NORMAL, &white);
  g_signal_connect (G_OBJECT (cache_canvas), "expose_event", G_CALLBACK (draw_cache_display), NULL);

  /* Place cache display into scrollable window */
  window = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(window), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(window), cache_canvas);

  return window;
}

/*****************************************************************************
   Replacement policy related functions
*****************************************************************************/

gboolean random_listener(GtkWidget* widget, gpointer data)
{
  if(GTK_TOGGLE_BUTTON(widget)->active)
    panel_replacement_policy = RANDOM;

  return TRUE;
}

gboolean lru_listener(GtkWidget* widget, gpointer data)
{
  if(GTK_TOGGLE_BUTTON(widget)->active)
    panel_replacement_policy = LRU;

  return TRUE;
}

gboolean lfu_listener(GtkWidget* widget, gpointer data)
{
  if(GTK_TOGGLE_BUTTON(widget)->active)
    panel_replacement_policy = LFU;

  return TRUE;
} 

GtkWidget* build_replace_policy_panel(void)
{
  GtkWidget* frame;
  GtkWidget* box;

  box = gtk_vbox_new(FALSE, 0);

  /* Build Random Policy radio button */
  random_policy_button = gtk_radio_button_new_with_label(NULL, "Random");
  g_signal_connect(G_OBJECT(random_policy_button), "clicked", G_CALLBACK(random_listener), NULL);

  /* Build LRU Policy radio button */
  lru_policy_button = gtk_radio_button_new_with_label(gtk_radio_button_get_group(GTK_RADIO_BUTTON(random_policy_button)), "LRU");
  g_signal_connect(G_OBJECT(lru_policy_button), "clicked", G_CALLBACK(lru_listener), NULL);

  /* Build LFU Policy radio button */
  lfu_policy_button = gtk_radio_button_new_with_label(gtk_radio_button_get_group(GTK_RADIO_BUTTON(random_policy_button)), "LFU");
  g_signal_connect(G_OBJECT(lfu_policy_button), "clicked", G_CALLBACK(lfu_listener), NULL);

  /* Pack the radio buttons */
  gtk_box_pack_start(GTK_BOX(box), lfu_policy_button, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(box), lru_policy_button, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(box), random_policy_button, TRUE, TRUE, 0);

  /* Initialize radio buttons */
  switch(panel_replacement_policy = policy)
  {
  case(RANDOM):
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(random_policy_button), TRUE);
    break;
  case(LRU):
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(lru_policy_button), TRUE);
    break;
  case(LFU):
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(lfu_policy_button), TRUE);
    break;
  default:
    printf("Impossible situation with policy: %u", policy);
  }

  /* Pack radio buttons */
  frame = gtk_frame_new("Replacement Policy");
  gtk_container_add(GTK_CONTAINER(frame), box);

  return frame;
}

/*****************************************************************************
   Memory Synchronization policy related functions
*****************************************************************************/

gboolean write_back_listener(GtkWidget* widget, gpointer data)
{
  if(GTK_TOGGLE_BUTTON(widget)->active)
    panel_memory_sync_policy = WRITE_BACK;

  return TRUE;
}

gboolean write_through_listener(GtkWidget* widget, gpointer data)
{
  if(GTK_TOGGLE_BUTTON(widget)->active)
    panel_memory_sync_policy = WRITE_THROUGH;

  return TRUE;
}

GtkWidget* build_memory_sync_policy_panel(void)
{
  GtkWidget* frame;
  GtkWidget* box;

  box = gtk_vbox_new(FALSE, 0);

  /* Build Write Back Policy radio button */
  write_back_policy_button = gtk_radio_button_new_with_label(NULL, "Write Back");
  g_signal_connect(G_OBJECT(write_back_policy_button), "clicked", G_CALLBACK(write_back_listener), NULL);

  /* Build Write Through radio button */
  write_through_policy_button = gtk_radio_button_new_with_label(gtk_radio_button_get_group(GTK_RADIO_BUTTON(write_back_policy_button)), "Write Through");
  g_signal_connect(G_OBJECT(write_through_policy_button), "clicked", G_CALLBACK(write_through_listener), NULL);

  /* Pack the radio buttons */
  gtk_box_pack_start(GTK_BOX(box), write_back_policy_button, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(box), write_through_policy_button, TRUE, TRUE, 0);

  /* Initialize radio buttons */
  switch(panel_memory_sync_policy = memory_sync_policy)
  {
  case(WRITE_BACK):
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(write_back_policy_button), TRUE);
    break;
  case(WRITE_THROUGH):
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(write_through_policy_button), TRUE);
    break;
  default:
    printf("Impossible situation with memory sync policy: %u", policy);
    exit(1);
  }

  /* Pack radio buttons */
  frame = gtk_frame_new("Memory Sync Policy");
  gtk_container_add(GTK_CONTAINER(frame), box);

  return frame;
}

/*****************************************************************************
   Cache View related functions
*****************************************************************************/

gboolean assoc_view_listener(GtkWidget* widget, gpointer data)
{
  if(GTK_TOGGLE_BUTTON(widget)->active)
    panel_cache_view = ASSOC;

  return TRUE;
}

gboolean index_view_listener(GtkWidget* widget, gpointer data)
{
  if(GTK_TOGGLE_BUTTON(widget)->active)
    panel_cache_view = INDEX;

  return TRUE;
}

GtkWidget* build_arrange_panel(void)
{
  GtkWidget* frame;
  GtkWidget* box;

  box = gtk_vbox_new(FALSE, 0);

  /* Build Write Back Policy radio button */
  index_view_button = gtk_radio_button_new_with_label(NULL, "Index (Set) Based");
  g_signal_connect(G_OBJECT(index_view_button), "clicked", G_CALLBACK(index_view_listener), NULL);

  /* Build Write Through radio button */
  assoc_view_button = gtk_radio_button_new_with_label(gtk_radio_button_get_group(GTK_RADIO_BUTTON(index_view_button)), "Associativity Based");
  g_signal_connect(G_OBJECT(assoc_view_button), "clicked", G_CALLBACK(assoc_view_listener), NULL);

  /* Pack the radio buttons */
  gtk_box_pack_start(GTK_BOX(box), index_view_button, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(box), assoc_view_button, TRUE, TRUE, 0);

  /* Initialize radio buttons */
  switch(panel_cache_view = view)
  {
  case(INDEX):
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(index_view_button), TRUE);
    break;
  case(ASSOC):
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(assoc_view_button), TRUE);
    break;
  default:
    printf("Impossible situation with cache view: %u", view);
    exit(1);
  }

  /* Pack radio buttons */
  frame = gtk_frame_new("Cache View Basis");
  gtk_container_add(GTK_CONTAINER(frame), box);

  return frame;
}

/*******************************************************************************
   Parameter panel related functions
 ******************************************************************************/

static gint assoc_entry_listener(GtkWidget* widget, GdkEventKey* event, gpointer func_data)
{
  GtkEntry *text;
  gboolean handled = FALSE;
  
  text = GTK_ENTRY (widget);
  
  /* intercept key presses */
  if(!(event->keyval >= GDK_KP_0 && event->keyval <= GDK_KP_9) && 
     !(event->keyval >= GDK_0 && event->keyval <= GDK_9) &&
     !(event->keyval == GDK_BackSpace) &&
     !(event->keyval == GDK_Delete) &&
     !(event->keyval == GDK_Tab))
  {
    gtk_signal_emit_stop_by_name (GTK_OBJECT (text), "key_press_event");
    handled = TRUE;
  }
  
  return handled;
}

static gint index_entry_listener(GtkWidget* widget, GdkEventKey* event, gpointer func_data)
{
  GtkEntry *text;
  gboolean handled = FALSE;
  
  text = GTK_ENTRY (widget);
  
  /* intercept key presses */
  if(!(event->keyval >= GDK_KP_0 && event->keyval <= GDK_KP_9) && 
     !(event->keyval >= GDK_0 && event->keyval <= GDK_9) &&
     !(event->keyval == GDK_BackSpace) &&
     !(event->keyval == GDK_Delete) &&
     !(event->keyval == GDK_Tab))
  {
    gtk_signal_emit_stop_by_name (GTK_OBJECT (text), "key_press_event");
    handled = TRUE;
  }

  return handled;
}

static gint block_entry_listener(GtkWidget* widget, GdkEventKey* event, gpointer func_data)
{
  GtkEntry *text;
  gboolean handled = FALSE;
  
  text = GTK_ENTRY (widget);
  
  /* intercept key presses */
  if(!(event->keyval >= GDK_KP_0 && event->keyval <= GDK_KP_9) && 
     !(event->keyval >= GDK_0 && event->keyval <= GDK_9) &&
     !(event->keyval == GDK_BackSpace) &&
     !(event->keyval == GDK_Delete) &&
     !(event->keyval == GDK_Tab))
  {
    gtk_signal_emit_stop_by_name (GTK_OBJECT (text), "key_press_event");
    handled = TRUE;
  }
  
  return handled;
}

GtkWidget* build_parameter_panel(void)
{
  GtkWidget* frame;

  GtkWidget* assoc_label;
  GtkWidget* index_label;
  GtkWidget* block_label;

  GtkWidget* label_box;
  GtkWidget* entry_box;
  GtkWidget* container;

  gchar buffer[10];
  GtkTooltips* parameter_panel_tooltips;

  /* Build containers */
  label_box = gtk_vbox_new(FALSE, 0);
  entry_box = gtk_vbox_new(FALSE, 0);
   
  /* Build fields and labels */
  assoc_entry = gtk_entry_new();
  sprintf(buffer, "%u", assoc);
  gtk_entry_set_text(GTK_ENTRY(assoc_entry), buffer);
  index_entry = gtk_entry_new();
  sprintf(buffer, "%u", set_count);
  gtk_entry_set_text(GTK_ENTRY(index_entry), buffer);
  block_entry = gtk_entry_new();
  sprintf(buffer, "%u", block_size);
  gtk_entry_set_text(GTK_ENTRY(block_entry), buffer);

  index_label = gtk_label_new("Number of Sets:");
  gtk_misc_set_alignment(GTK_MISC(index_label), 1, .5) ;
  assoc_label = gtk_label_new("Associativity:");
  gtk_misc_set_alignment(GTK_MISC(assoc_label), 1, .5) ;
  block_label = gtk_label_new("Block Size (bytes):");
  gtk_misc_set_alignment(GTK_MISC(block_label), 1, .5) ;

  parameter_panel_tooltips = gtk_tooltips_new();
  gtk_tooltips_set_tip(GTK_TOOLTIPS(parameter_panel_tooltips), assoc_entry, "Sets the associativity of the cache\n(i.e. how many places can a memory block map to using its index)", NULL);
  gtk_tooltips_set_tip(GTK_TOOLTIPS(parameter_panel_tooltips), index_entry, "Set the number of sets (i.e. the number of unique indexes)", NULL);
  gtk_tooltips_set_tip(GTK_TOOLTIPS(parameter_panel_tooltips), block_entry, "Set the number of bytes in a cache block", NULL);

  /* Set event handlers */
  gtk_signal_connect (GTK_OBJECT (assoc_entry), "key_press_event", GTK_SIGNAL_FUNC(assoc_entry_listener), NULL);
  gtk_signal_connect (GTK_OBJECT (index_entry), "key_press_event", GTK_SIGNAL_FUNC(index_entry_listener), NULL);
  gtk_signal_connect (GTK_OBJECT (block_entry), "key_press_event", GTK_SIGNAL_FUNC(block_entry_listener), NULL);

  /* Place fields and labels into containers */
  gtk_box_pack_start(GTK_BOX(label_box), assoc_label, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(label_box), index_label, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(label_box), block_label, TRUE, TRUE, 0);

  gtk_box_pack_start(GTK_BOX(entry_box), assoc_entry, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(entry_box), index_entry, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(entry_box), block_entry, TRUE, TRUE, 0);

  /* Place boxes into containing container */
  container = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(container), label_box, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(container), entry_box, TRUE, TRUE, 0);

  /* Frame container */
  frame = gtk_frame_new("Cache Parameters");
  gtk_container_add(GTK_CONTAINER(frame), container);

  return frame;
}

/******************************************************************************
   Button Panel related functions
 *****************************************************************************/

gboolean configure_button_listener(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  GtkWidget* dialog;
  GtkWidget* arrange_panel;
  GtkWidget* separator3;
  GtkWidget* parameter_panel;
  GtkWidget* separator1;
  GtkWidget* replacement_panel;
  GtkWidget* separator2;
  GtkWidget* sync_panel;
  gint result;
 
  gchar buffer[200];  
  
  /* Create the widgets */
  dialog = gtk_dialog_new_with_buttons ("Configure Cache",
					GTK_WINDOW(main_window),
					GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
					GTK_STOCK_OK,
					GTK_RESPONSE_ACCEPT,
					GTK_STOCK_CANCEL,
					GTK_RESPONSE_REJECT,
					NULL);
  
  arrange_panel = build_arrange_panel();
  separator3 = gtk_hseparator_new();
  parameter_panel = build_parameter_panel();
  separator1 = gtk_hseparator_new();
  gtk_widget_set_size_request(separator1, 100, 10);
  replacement_panel = build_replace_policy_panel();
  separator2 = gtk_hseparator_new();
  gtk_widget_set_size_request(separator2, 100, 10);
  sync_panel = build_memory_sync_policy_panel();
   
  gtk_container_add (GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), arrange_panel);
  gtk_container_add (GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), separator3);
  gtk_container_add (GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), parameter_panel);
  gtk_container_add (GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), separator1);
  gtk_container_add (GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), replacement_panel);
  gtk_container_add (GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), separator2);
  gtk_container_add (GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), sync_panel);

  gtk_widget_show_all (dialog);

  /* Block for user response */
  result = gtk_dialog_run(GTK_DIALOG(dialog));

  switch(result)
  {
  case GTK_RESPONSE_ACCEPT:
    validate_cache_parameters(atoi(gtk_entry_get_text(GTK_ENTRY(index_entry))),
			      atoi(gtk_entry_get_text(GTK_ENTRY(assoc_entry))),
			      atoi(gtk_entry_get_text(GTK_ENTRY(block_entry))));
    assert(panel_replacement_policy == RANDOM || panel_replacement_policy == LRU || panel_replacement_policy == LFU);
    policy = panel_replacement_policy;
    assert(panel_memory_sync_policy == WRITE_BACK || panel_memory_sync_policy == WRITE_THROUGH);
    memory_sync_policy = panel_memory_sync_policy;
    assert(panel_cache_view == INDEX || panel_cache_view == ASSOC);
    view = panel_cache_view;

    sprintf(buffer, "Cache parameters changed:\n + set count = %d\n + associativity = %d\n + block size = %d\n + replacement policy = %s\n + memory sync policy = %s\n", set_count, assoc, block_size, (policy == RANDOM ? "Random" : (policy == LRU ? "LRU" : "LFU")), (memory_sync_policy == WRITE_BACK ? "Write Back" : "Write Through"));
    append_log(buffer);
    configure_cache_drawing_parameters(cache_canvas);
    flush_cache();
    flush_drawlist();
    refresh_cache_display();
    break;
  case GTK_RESPONSE_REJECT:
    /* printf("do not update cache"); */
    break;
  default:
    printf("How was this reached?: %d", result);
  }

  /* Destroy dialog */
  gtk_widget_destroy (dialog);
  return TRUE;
}

gboolean display_font_dialog(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  GtkWidget *dialog;
  dialog = gtk_font_selection_dialog_new("Foobar");
  gtk_dialog_run (GTK_DIALOG (dialog));

  printf(gtk_font_selection_dialog_get_font_name(GTK_FONT_SELECTION_DIALOG(dialog)));
  gtk_widget_destroy(dialog);

  return TRUE;
}

/* Remove function when GTK API supports file_chooser */
void store_filename (GtkWidget *widget, gpointer user_data) {
   GtkWidget *file_chooser = GTK_WIDGET (user_data);

   const char* selected_filename;

   selected_filename = gtk_file_selection_get_filename (GTK_FILE_SELECTION (file_chooser));
   load_dumpfile(selected_filename);
}

gboolean load_button_listener(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  GtkWidget *dialog;

  dialog = gtk_file_chooser_dialog_new ("Open File",
					GTK_WINDOW(main_window),
					GTK_FILE_CHOOSER_ACTION_OPEN,
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
					NULL);

  /* Adding file filter for *.dump files */
  GtkFileFilter *dump_filter = gtk_file_filter_new();
  gtk_file_filter_add_pattern(dump_filter, "*.dump");
  gtk_file_filter_set_name(dump_filter, "Dump Files");

  GtkFileFilter *all_filter = gtk_file_filter_new();
  gtk_file_filter_add_pattern(all_filter, "*.*");
  gtk_file_filter_set_name(all_filter, "All Files");

  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), dump_filter);
  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog), all_filter);

  /* Show dialog */
  if (gtk_dialog_run (GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT)
  {
    char *filename;
    filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
    load_dumpfile(filename);
    g_free (filename);
  }

  gtk_file_chooser_remove_filter(GTK_FILE_CHOOSER(dialog), dump_filter);
  gtk_file_chooser_remove_filter(GTK_FILE_CHOOSER(dialog), all_filter);
  gtk_widget_destroy (dialog);

  return TRUE;
}

gboolean step_button_listener(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  step_processor();
  return TRUE;
}

gboolean call_step_processor(gpointer data)
{
  step_processor();
  return TRUE;
}

gboolean start_button_listener(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  if(timer_active == FALSE)
  {
    timer_id = g_timeout_add(gtk_range_get_value(GTK_RANGE(speed_slider)), call_step_processor, NULL);
    timer_active = TRUE;
  }
  else
  {
    g_source_remove(timer_id);
    timer_id = g_timeout_add(gtk_range_get_value(GTK_RANGE(speed_slider)), call_step_processor, NULL);
  }

  return TRUE;
}

gboolean speed_slider_listener(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  if(timer_active == TRUE)
  {
    stop_run();
    timer_id = g_timeout_add(gtk_range_get_value(GTK_RANGE(speed_slider)), call_step_processor, NULL);
    timer_active = TRUE;
  }

  return TRUE;
}

void stop_run()
{
  if(timer_active == TRUE)
  {
    g_source_remove(timer_id);
    timer_active = FALSE;
  }
}

gboolean stop_button_listener(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  stop_run();
  return TRUE;
}

gboolean run_button_listener(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  GtkWidget* dialog;  
  GtkWidget* run_panel;
  GtkWidget* min_speed_label; 
  GtkWidget* max_speed_label;
  GtkWidget* start_button;
  GtkWidget* stop_button;
  gchar buffer[15];

  /* Build Dialog */
  dialog = gtk_dialog_new_with_buttons ("Test Cache",
					GTK_WINDOW(main_window),
					GTK_DIALOG_DESTROY_WITH_PARENT,
					"Done",
					GTK_RESPONSE_ACCEPT,
					NULL);

  /* Build widgets that will go into dialog */
  sprintf(buffer, "%d ms", MIN_SPEED);
  min_speed_label = gtk_label_new(buffer);
  speed_slider = gtk_hscale_new_with_range(MIN_SPEED, MAX_SPEED, 10);
  sprintf(buffer, "%d ms", MAX_SPEED);
  max_speed_label = gtk_label_new(buffer);
  gtk_range_set_value(GTK_RANGE(speed_slider), 1000);
  gtk_widget_set_size_request(speed_slider, 200, 40);
  start_button = gtk_button_new_with_label("Start");
  stop_button = gtk_button_new_with_label("Stop");

  /* Give widgets some functionality */
  gtk_signal_connect(GTK_OBJECT(GTK_HSCALE(speed_slider)->scale.range.adjustment), "value_changed", GTK_SIGNAL_FUNC(speed_slider_listener), NULL);  
  g_signal_connect(G_OBJECT(start_button), "clicked", G_CALLBACK(start_button_listener), NULL);
  g_signal_connect(G_OBJECT(stop_button), "clicked", G_CALLBACK(stop_button_listener), NULL);
  
  /* Arrange widgets that will gointo dialog */
  run_panel = gtk_table_new(20, 4, FALSE);
  gtk_table_attach_defaults(GTK_TABLE(run_panel), gtk_label_new("To start the test, click on \"Start\".\nIf you want to change speed you must click \"Start\" again."), 0, 20, 0, 2);
  gtk_table_attach_defaults(GTK_TABLE(run_panel), min_speed_label, 0, 1, 2, 3);
  gtk_table_attach_defaults(GTK_TABLE(run_panel), speed_slider, 1, 19, 2, 3);
  gtk_table_attach_defaults(GTK_TABLE(run_panel), max_speed_label, 19, 20, 2, 3);
  gtk_table_attach_defaults(GTK_TABLE(run_panel), start_button, 0, 10, 3, 4);
  gtk_table_attach_defaults(GTK_TABLE(run_panel), stop_button, 10, 20, 3, 4);
  gtk_widget_set_size_request(run_panel, 400, 160);

  /* Place widgets into dialog and display */
  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), run_panel);
  gtk_widget_show_all (dialog);

  /* Block for user response */
  gtk_dialog_run(GTK_DIALOG(dialog));

  if(timer_active == TRUE)
  {
    g_source_remove(timer_id);
    timer_active = FALSE;
  }

  gtk_widget_destroy(dialog);

  return TRUE;
}

gboolean reset_machine_button_listener(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  reinit_processor();
  append_log("--PC reset--\n");
  return TRUE;
}

gboolean reset_cache_button_listener(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  flush_cache();
  refresh_cache_display();
  append_log("--Cache flushed--\n");
  return TRUE;
}

gboolean reset_output_button_listener(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  GtkTextBuffer* buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textbox));
  gtk_text_buffer_set_text(buffer, "--Output Cleared--\n", -1);

  return TRUE;
}

GtkWidget* build_button_panel()
{
  GtkWidget* panel;

  GtkWidget* configure_frame;
  GtkWidget* configure_button;

  GtkWidget* op_frame;
  GtkWidget* load_button;
  GtkWidget* step_button;
  GtkWidget* run_button;

  GtkWidget* reset_frame;
  GtkWidget* reset_machine_button;
  GtkWidget* reset_cache_button;
  GtkWidget* reset_output_button;

  GtkWidget* quit_frame;
  GtkWidget* quit_button;

  GtkWidget* test_table;
  GtkWidget* reset_table;

  GtkTooltips* button_panel_tooltips;

  /* Build buttons */
  configure_button = gtk_button_new_with_label("Config Cache");
  load_button = gtk_button_new_with_label("Load Program");
  step_button = gtk_button_new_with_label("Step");
  run_button = gtk_button_new_with_label("Run");
  reset_machine_button = gtk_button_new_with_label("CPU");
  reset_cache_button = gtk_button_new_with_label("Cache");
  reset_output_button = gtk_button_new_with_label("Output");
  quit_button = gtk_button_new_with_label("Quit");

  /* Attach tooltips */
  button_panel_tooltips = gtk_tooltips_new();
  gtk_tooltips_set_tip(GTK_TOOLTIPS(button_panel_tooltips), configure_button, "Configure various parameters of the cache.", NULL);
  gtk_tooltips_set_tip(GTK_TOOLTIPS(button_panel_tooltips), load_button, "Loads a new file and resets CPU", NULL);
  gtk_tooltips_set_tip(GTK_TOOLTIPS(button_panel_tooltips), step_button, "Execute only the next instruction of the loaded program", NULL);
  gtk_tooltips_set_tip(GTK_TOOLTIPS(button_panel_tooltips), run_button, "Execute the loaded program", NULL);
  gtk_tooltips_set_tip(GTK_TOOLTIPS(button_panel_tooltips), reset_machine_button, "Reset the CPU to allow re-execution of loaded program", NULL);
  gtk_tooltips_set_tip(GTK_TOOLTIPS(button_panel_tooltips), reset_cache_button, "Flushes the contents of the cache", NULL);
  gtk_tooltips_set_tip(GTK_TOOLTIPS(button_panel_tooltips), reset_output_button, "Clears the log", NULL);
  gtk_tooltips_set_tip(GTK_TOOLTIPS(button_panel_tooltips), quit_button, "Quit the program", NULL);

  /* Build button functions */
  g_signal_connect(G_OBJECT(configure_button), "clicked", G_CALLBACK(configure_button_listener), NULL);
  g_signal_connect(G_OBJECT(load_button), "clicked", G_CALLBACK(load_button_listener), NULL);
  g_signal_connect(G_OBJECT(step_button), "clicked", G_CALLBACK(step_button_listener), NULL);
  g_signal_connect(G_OBJECT(run_button), "clicked", G_CALLBACK(run_button_listener), NULL);
  g_signal_connect(G_OBJECT(reset_machine_button), "clicked", G_CALLBACK(reset_machine_button_listener), NULL);
  g_signal_connect(G_OBJECT(reset_cache_button), "clicked", G_CALLBACK(reset_cache_button_listener), NULL);
  g_signal_connect(G_OBJECT(reset_output_button), "clicked", G_CALLBACK(reset_output_button_listener), NULL);
  g_signal_connect_swapped(G_OBJECT(quit_button), "clicked", G_CALLBACK(gtk_widget_destroy), G_OBJECT(main_window));

  /* Build table */
  test_table = gtk_table_new(1, 3, TRUE);
  gtk_table_attach_defaults(GTK_TABLE(test_table), load_button, 0, 1, 0, 1);
  gtk_table_attach_defaults(GTK_TABLE(test_table), step_button, 1, 2, 0, 1);
  gtk_table_attach_defaults(GTK_TABLE(test_table), run_button, 2, 3, 0, 1);

  reset_table = gtk_table_new(1, 3, TRUE);
  gtk_table_attach_defaults(GTK_TABLE(reset_table), reset_machine_button, 0, 1, 0, 1);
  gtk_table_attach_defaults(GTK_TABLE(reset_table), reset_cache_button, 1, 2, 0, 1);
  gtk_table_attach_defaults(GTK_TABLE(reset_table), reset_output_button, 2, 3, 0, 1);

  /* Frame Containers */
  configure_frame = gtk_frame_new("Setup");
  gtk_container_add(GTK_CONTAINER(configure_frame), configure_button);

  op_frame = gtk_frame_new("Test");
  gtk_container_add(GTK_CONTAINER(op_frame), test_table);

  reset_frame = gtk_frame_new("Reset");
  gtk_container_add(GTK_CONTAINER(reset_frame), reset_table);

  quit_frame = gtk_frame_new("Quit");
  gtk_container_add(GTK_CONTAINER(quit_frame), quit_button);

  panel = gtk_table_new(1, 7, TRUE);
  gtk_table_attach(GTK_TABLE(panel), configure_frame, 0, 1, 0, 1, GTK_FILL, GTK_SHRINK, 0, 0);
  gtk_table_attach(GTK_TABLE(panel), op_frame, 1, 4, 0, 1, GTK_FILL, GTK_SHRINK, 0, 0);
  gtk_table_attach(GTK_TABLE(panel), reset_frame, 4, 7, 0, 1, GTK_FILL, GTK_SHRINK, 0, 0);
  gtk_table_attach(GTK_TABLE(panel), quit_frame, 7, 8, 0, 1, GTK_FILL, GTK_SHRINK, 0, 0);

  return panel;
}

/******************************************************************************
   Main GUI related functions
 *****************************************************************************/

void exit_program(GtkWidget* widget, gpointer data)
{
  /* free data */
  if(fontdesc != NULL)
    pango_font_description_free(fontdesc);

  if(layout != NULL)
    g_object_unref(layout);

  gtk_main_quit();
}

void init_colors()
{
  cmap = gdk_screen_get_default_colormap(gdk_screen_get_default());
  gdk_colormap_alloc_color(cmap, &red, FALSE, TRUE);
  gdk_color_parse("#ff0000", &red);

  gdk_colormap_alloc_color(cmap, &white, FALSE, TRUE);
  gdk_color_parse("#ffffff", &white);

  gdk_colormap_alloc_color(cmap, &skyblue, FALSE, TRUE);
  gdk_color_parse("#87ceeb", &skyblue);

  gdk_colormap_alloc_color(cmap, &palegreen, FALSE, TRUE);
  gdk_color_parse("#98fb98", &palegreen);

  gdk_colormap_alloc_color(cmap, &black, FALSE, TRUE);
  gdk_color_parse("#000000", &black);
}

int build_gui(int argc, char** argv)
{
  GtkWidget* main_layout;
  GtkWidget* drawing_panel;
  GtkWidget* regcache_vpaned;

  GtkWidget* reglog_hpaned;
  GtkWidget* register_panel;
  GtkWidget* log_panel;

  GtkWidget* button_panel;

  /* Do gtk initializations, if any */
  gtk_init (&argc, &argv);
  init_colors();
  layout = NULL;
  fontdesc = NULL;
  base_display_variables_initialized = FALSE;
  timer_active = FALSE;
  drawlist = NULL;

  /* Initialize window */
  main_window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_default_size(GTK_WINDOW(main_window), WINDOW_DEFAULT_WIDTH, WINDOW_DEFAULT_HEIGHT);
  gtk_window_set_resizable(GTK_WINDOW(main_window), TRUE);
  gtk_window_set_title(GTK_WINDOW(main_window), argv[0]);
  gtk_container_set_border_width (GTK_CONTAINER(main_window), 5);
  g_signal_connect (G_OBJECT (main_window), "destroy", G_CALLBACK(exit_program), NULL);    

  /*  Build layout */  
  main_layout = gtk_vbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(main_window), main_layout);

  /* Build panels */
  register_panel = build_register_panel();
  log_panel = build_log_panel();

  reglog_hpaned = gtk_hpaned_new();
  gtk_paned_add1(GTK_PANED(reglog_hpaned), register_panel);
  gtk_paned_add2(GTK_PANED(reglog_hpaned), log_panel);

  drawing_panel = build_drawing_panel();
  button_panel = build_button_panel();

  regcache_vpaned = gtk_vpaned_new();
  gtk_paned_add1(GTK_PANED(regcache_vpaned), reglog_hpaned);
  gtk_paned_add2(GTK_PANED(regcache_vpaned), drawing_panel);

  /* Set sizes of panels */
  gtk_widget_set_size_request(register_panel, WINDOW_DEFAULT_WIDTH * .50, WINDOW_DEFAULT_HEIGHT * .25);
  gtk_widget_set_size_request(log_panel, WINDOW_DEFAULT_WIDTH * .50, WINDOW_DEFAULT_HEIGHT * .25);
  gtk_widget_set_size_request(drawing_panel, WINDOW_DEFAULT_WIDTH, WINDOW_DEFAULT_HEIGHT * .68);

  /* Place panels into window */  
  gtk_box_pack_start(GTK_BOX(main_layout), regcache_vpaned, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(main_layout), button_panel, FALSE, FALSE, 0);

  /* Display window */
  gtk_widget_show_all(main_window);    

  /* Load file if any */
  if(argc >= 2)
    load_dumpfile(argv[argc - 1]);

  gtk_main();
    
  return 0;
}

/******************************************************************************
   Cache Highlighting functions
 *****************************************************************************/

void highlight_block(unsigned int set_num, unsigned int assoc_num) 
{ 
  node* new_item;

  new_item = (node*)(malloc(sizeof(node)));
  new_item->type = HIGHLIGHT_BLOCK;
  new_item->x = base_x_offset;

  switch(view)
  {
  case INDEX:
    new_item->y = base_y_offset + cache_header_height + set_num * (line_height + (line_height * assoc)) + (assoc_num * line_height);
    break;
  case ASSOC:
    new_item->y = base_y_offset + 
                  (assoc_num * (cache_header_height + line_height + cache_unit_height + ((set_count / 4) * line_height))) + 
                  cache_header_height + 
                  (set_num * line_height) + 
                  ((set_num / 4) * line_height);
    break;
  default:
    printf("Invalid arrange mode");
    exit(1);
  }    
      
  new_item->width = horizontal_line_width;
  new_item->height = line_height;
  new_item->next = drawlist;

  drawlist = new_item;
}

void highlight_offset(unsigned int set_num, unsigned int assoc_num, unsigned int offset, CacheAction action)
{
  node* new_item;

  new_item = (node*)(malloc(sizeof(node)));
  new_item->type = HIGHLIGHT_OFFSET;
  new_item->unit_index = assoc_num;
  new_item->block_index = set_num;
  new_item->block_offset = offset;

  switch(action)
  {
  case HIT:
    new_item->fg_color = NULL;
    new_item->bg_color = &palegreen;
    break;
  case MISS:
    new_item->fg_color = &white;
    new_item->bg_color = &red;
    break;
  default:
    printf("Impossible cache action");
    exit(1);
  }

#ifdef CYGWIN
  new_item->x = base_x_offset + block_header_width + (offset * byte_width) + ((offset / 4) * (char_width * 3)) + (((offset / 2) * char_width) - ((offset / 4) * char_width));
  new_item->width = (sizeof(instruction) * byte_width) + char_width;
#else
  new_item->x = base_x_offset + block_header_width + (offset * byte_width) + ((offset / 4) * char_width);
  new_item->width = (sizeof(instruction) * byte_width);
#endif

  switch(view)
  {
  case INDEX:
    new_item->y = base_y_offset + cache_header_height + set_num * (line_height + (line_height * assoc)) + (assoc_num * line_height);
    break;
  case ASSOC:
    new_item->y = base_y_offset + 
                  (assoc_num * (cache_header_height + line_height + cache_unit_height + ((set_count / 4) * line_height))) + 
                  cache_header_height + 
                  (set_num * line_height) + 
                  ((set_num / 4) * line_height);
    break;
  default:
    printf("Invalid arrange mode");
    exit(1);
  }    

  new_item->height = line_height;
  new_item->next = drawlist;

  drawlist = new_item; 
}
