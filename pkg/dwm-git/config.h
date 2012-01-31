// Makes XF86* keys work
#include <X11/XF86keysym.h>

// Appearance
#define NUMCOLORS 8
static const char colors[NUMCOLORS][ColLast][8] = {
	// border    foreground background
	{ "#1a1a1a", "#888888", "#1a1a1a" },  // normal           light gray / dark gray
	{ "#82ca42", "#ffffff", "#82ca42" },  // selected tag     white      / light green
	{ "#fd5337", "#ffffff", "#fd5337" },  // urgent/warning   white      / salmon red
	{ "#fda12b", "#ffffff", "#fda12b" },  // error            white      / orange
	{ "#000000", "#90d94e", "#1a1a1a" },  // layout icon      green      / dark gray
	{ "#000000", "#ffffff", "#1a1a1a" },  // white            white      / dark gray
	{ "#000000", "#fd5337", "#1a1a1a" },  // red warning fg   red        / dark gray
	{ "#000000", "#daff30", "#1a1a1a" },  //
};

static const char font[] =
                         "-lokaltog-symbols-*-*-*-*-11-*-*-*-*-*-*-*"
                         ","
                         "-lokaltog-fixed-medium-*-*-*-10-*-*-*-*-*-iso8859-*"
                         ;

static const unsigned int borderpx  = 1;        // border pixel of windows
static const unsigned int snap      = 32;       // snap pixel
static const Bool showbar           = True;     // False means no bar
static const Bool topbar            = True;     // False means bottom bar

// Layout(s)
static const float mfact      = 0.68;   // factor of master area size [0.05..0.95]
static const Bool resizehints = False;  // True means respect size hints in tiled resizals

static const Layout layouts[] = {
	// symbol   arrange function
	{ "",      tile        }, // []=  first entry is default
	{ "",      NULL        }, // <><  no layout function means floating behavior
	{ "",      monocle     }, // [M]
	{ "",      gaplessgrid }, // [#]
	{ "",      bstack      }, // [TTT]
};

// Tags
static const Tag tags[] = {
	// name         layout           mfact
	{ " Main",     &layouts[0],     -1   },
	{ " Web",      &layouts[0],     0.5  },
	{ " Dev",      &layouts[0],     0.8  },
	{ " Media",    &layouts[0],     0.8  },
	{ " IM",       &layouts[2],     -1   },
};

// Window rules
static const Rule rules[] = {
	// class      instance    title       tags mask     isfloating   monitor
	{ "Gimp",     NULL,       NULL,       0,            True,        -1 },
	{ "feh",      NULL,       NULL,       0,            True,        -1 },
	{ "MPlayer",  NULL,       NULL,       0,            True,        -1 },
	{ "Chromium", NULL,       NULL,       1<<1,         False,       -1 },
	{ "URxvt",    "vim",      NULL,       0,            False,       -1 },
	{ "URxvt",    "ncmpcpp",  NULL,       1<<3,         False,       -1 },
	{ "URxvt",    "ranger",   NULL,       0,            False,       -1 },
	{ "URxvt",    "weechat",  NULL,       1<<4,         False,       -1 },
};

// Restart function
void
restart(const Arg *arg)
{
	if (arg->v) {
		execvp(((char **)arg->v)[0], (char **)arg->v);
	} else {
		execlp("dwm", "dwm", NULL);
	}
}

// Key definitions
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

static const char terminal[] = "urxvtc";

// Commands
static const char *term_cmd[]                 = { terminal, NULL };
static const char *dmenu_cmd[]                = { "dmenu_run", "-fn", font, "-nb", colors[0][ColBG], "-nf", colors[0][ColFG], "-sb", colors[1][ColBG], "-sf", colors[1][ColFG], NULL };
static const char *screenshot_cmd[]           = { "scrot", NULL };

static const char *run_chromium[]             = { "/home/kim/sync/bin/chromium",     NULL };
static const char *run_chromium_tor[]         = { "/home/kim/sync/bin/chromium-tor", NULL };
static const char *run_minecraft[]            = { "/home/kim/sync/bin/minecraft",    NULL };

static const char *run_vim[]                  = { terminal, "-name", "vim",        "-e", "vim",                NULL };
static const char *run_ncmpcpp[]              = { terminal, "-name", "ncmpcpp",    "-e", "ncmpcpp",            NULL };
static const char *run_ranger[]               = { terminal, "-name", "ranger",     "-e", "ranger",             NULL };
static const char *run_weechat[]              = { terminal, "-name", "weechat",    "-e", "weechat-curses",     NULL };

static const char *control_volume_lower[]     = { "amixer", "set", "Master", "10-%", NULL };
static const char *control_volume_raise[]     = { "amixer", "set", "Master", "10+%", NULL };

static const char *control_shutdown[]         = { "sudo", "shutdown", "-h", "now", NULL };
static const char *control_reboot[]           = { "sudo", "shutdown", "-r", "now", NULL };

static Key keys[] = {
	// modifier               key           function          argument
	{ MODKEY,                 XK_x,         spawn,            {.v = dmenu_cmd } },
	{ MODKEY,                 XK_Return,    spawn,            {.v = term_cmd } },
	{ MODKEY,                 XK_m,         spawn,            {.v = run_minecraft } },
	{ MODKEY|ShiftMask,       XK_m,         spawn,            {.v = run_ncmpcpp } },
	{ MODKEY,                 XK_n,         spawn,            {.v = run_chromium } },
	{ MODKEY|ShiftMask,       XK_n,         spawn,            {.v = run_chromium_tor } },
	{ MODKEY,                 XK_v,         spawn,            {.v = run_vim } },
	{ MODKEY,                 XK_r,         spawn,            {.v = run_ranger } },
	{ MODKEY,                 XK_c,         spawn,            {.v = run_weechat } },

	{ MODKEY,                 XK_h,         setmfact,         {.f = -0.05} },
	{ MODKEY,                 XK_j,         focusstack,       {.i = -1 } },
	{ MODKEY,                 XK_k,         focusstack,       {.i = +1 } },
	{ MODKEY,                 XK_l,         setmfact,         {.f = +0.05} },

	{ MODKEY,                 XK_Tab,       view,             {0} },
	{ MODKEY,                 XK_Escape,    killclient,       {0} },
	{ MODKEY,                 XK_b,         togglebar,        {0} },
	{ MODKEY,                 XK_space,     setlayout,        {0} },
	{ MODKEY|ShiftMask,       XK_space,     togglefloating,   {0} },
	{ MODKEY,                 XK_comma,     focusmon,         {.i = -1 } },
	{ MODKEY,                 XK_period,    focusmon,         {.i = +1 } },
	{ MODKEY|ShiftMask,       XK_comma,     tagmon,           {.i = -1 } },
	{ MODKEY|ShiftMask,       XK_period,    tagmon,           {.i = +1 } },

	{ 0,                      XK_Print,     spawn,            {.v = screenshot_cmd } },
	{ Mod1Mask,               XK_Return,    zoom,             {0} },

	TAGKEYS(                  XK_a,                           0)
	TAGKEYS(                  XK_o,                           1)
	TAGKEYS(                  XK_e,                           2)
	TAGKEYS(                  XK_u,                           3)
	TAGKEYS(                  XK_i,                           4)

	{ MODKEY|ControlMask,     XK_t,         setlayout,        {.v = &layouts[0]} }, // (t)ile
	{ MODKEY|ControlMask,     XK_f,         setlayout,        {.v = &layouts[1]} }, // (f)loating
	{ MODKEY|ControlMask,     XK_m,         setlayout,        {.v = &layouts[2]} }, // (m)onocle
	{ MODKEY|ControlMask,     XK_g,         setlayout,        {.v = &layouts[3]} }, // (g)aplessgrid
	{ MODKEY|ControlMask,     XK_b,         setlayout,        {.v = &layouts[4]} }, // (b)stack

	{ MODKEY|ShiftMask,       XK_Escape,    quit,             {0} },
	{ MODKEY|ShiftMask,       XK_r,         restart,          {0} },
	{ MODKEY|ControlMask,     XK_Escape,    spawn,            {.v = control_shutdown } },
	{ MODKEY|ControlMask,     XK_r,         spawn,            {.v = control_reboot } },

	{ 0,                      XF86XK_AudioLowerVolume,   spawn, {.v = control_volume_lower } },
	{ 0,                      XF86XK_AudioRaiseVolume,   spawn, {.v = control_volume_raise } },
};

// Button definitions
// Click can be ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin
static Button buttons[] = {
	// click                event mask      button          function        argument
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};
