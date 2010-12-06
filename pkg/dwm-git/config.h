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
static const char font[]            = "-artwiz-cureextra-medium-*-*-*-*-*-*-*-*-*-*-*";
static const unsigned int borderpx  = 1;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const Bool showbar           = True;     /* False means no bar */
static const Bool topbar            = True;     /* False means bottom bar */

// Layout(s)
static const float mfact      = 0.68;   // factor of master area size [0.05..0.95]
static const Bool resizehints = False;  // True means respect size hints in tiled resizals

static const Layout layouts[] = {
	// symbol   arrange function
	{ "½",      tile        }, // []=  first entry is default
	{ "¾",      NULL        }, // <><  no layout function means floating behavior
	{ "¼",      monocle     }, // [M]
	{ "¿",      gaplessgrid }, // [#]
	{ "À",      bstack      }, // [TTT]
};

// Tags
static const Tag tags[] = {
	// name       layout           mfact
	{ "Ä Main",     &layouts[0],     -1   },
	{ "Ã Web",      &layouts[0],     0.5  },
	{ "Â Dev",      &layouts[4],     0.7  },
	{ "Ç Misc",     &layouts[3],     -1   },
	{ "Á IM",       &layouts[2],     -1   },
};

// Window rules
static const Rule rules[] = {
	// class      instance    title       tags mask     isfloating   monitor
	{ "Gimp",     NULL,       NULL,       0,            True,        -1 },
	{ "feh",      NULL,       NULL,       0,            True,        -1 },
	{ "MPlayer",  NULL,       NULL,       0,            True,        -1 },
	{ "Chromium", NULL,       NULL,       1<<1,         False,       -1 },
	{ "URxvt",    "vim",      NULL,       1<<2,         False,       -1 },
	{ "URxvt",    "ncmpcpp",  NULL,       0,            False,       -1 },
	{ "URxvt",    "ranger",   NULL,       0,            False,       -1 },
	{ "URxvt",    "mutt",     NULL,       1<<3,         False,       -1 },
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

static const char terminal[]  = "urxvtc";

// Commands
static const char *dmenucmd[] = { "dmenu_run", "-fn", font, "-nb", colors[0][ColBG], "-nf", colors[0][ColFG], "-sb", colors[1][ColBG], "-sf", colors[1][ColFG], NULL };
static const char *chromium[] = { "chromium", NULL };
static const char *termcmd[]  = { terminal, NULL };
static const char *vim[]      = { terminal, "-name", "vim",     "-e", "vim",            NULL };
static const char *ncmpcpp[]  = { terminal, "-name", "ncmpcpp", "-e", "ncmpcpp",        NULL };
static const char *ranger[]   = { terminal, "-name", "ranger",  "-e", "ranger",         NULL };
static const char *mutt[]     = { terminal, "-name", "mutt",    "-e", "mutt",           NULL };
static const char *weechat[]  = { terminal, "-name", "weechat", "-e", "weechat-curses", NULL };
static const char *scrot[]    = { "scrot", NULL };

static Key keys[] = {
	// modifier                     key        function        argument
	{ MODKEY,                       XK_x,      spawn,          {.v = dmenucmd } },
	{ MODKEY,                       XK_Return, spawn,          {.v = termcmd } },
	{ MODKEY,                       XK_n,      spawn,          {.v = chromium } },
	{ MODKEY,                       XK_v,      spawn,          {.v = vim } },
	{ MODKEY|ShiftMask,             XK_n,      spawn,          {.v = ncmpcpp } },
	{ MODKEY,                       XK_r,      spawn,          {.v = ranger } },
	{ MODKEY|ShiftMask,             XK_m,      spawn,          {.v = mutt } },
	{ MODKEY,                       XK_c,      spawn,          {.v = weechat } },
	{ 0,                            XK_Print,  spawn,          {.v = scrot } },

	{ MODKEY,                       XK_Up,     focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_Down,   focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_Right,  setmfact,       {.f = +0.05} },
	{ MODKEY,                       XK_Left,   setmfact,       {.f = -0.05} },

	{ MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_j,      focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_k,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },

	{ Mod1Mask,                     XK_Return, zoom,           {0} },
	{ MODKEY,                       XK_Tab,    view,           {0} },
	{ MODKEY,                       XK_Escape, killclient,     {0} },
	{ MODKEY,                       XK_b,      togglebar,      {0} },
	{ MODKEY,                       XK_space,  setlayout,      {0} },
	{ MODKEY|ShiftMask,             XK_space,  togglefloating, {0} },
	{ MODKEY,                       XK_0,      view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
	{ MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_period, focusmon,       {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },

	TAGKEYS(                        XK_a,                      0)
	TAGKEYS(                        XK_o,                      1)
	TAGKEYS(                        XK_e,                      2)
	TAGKEYS(                        XK_u,                      3)
	TAGKEYS(                        XK_i,                      4)

	{ MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
	{ MODKEY,                       XK_g,      setlayout,      {.v = &layouts[3]} },

	{ MODKEY|ShiftMask,             XK_Escape, quit,           {0} },
	{ MODKEY|ShiftMask,             XK_r,      restart,        {0} },
};

// Button definitions
// Click can be ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin
static Button buttons[] = {
	// click                event mask      button          function        argument
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};
