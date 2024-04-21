# NOTE: config.py is intended for advanced users who are comfortable
# with manually migrating the config file on qutebrowser upgrades. If
# you prefer, you can also configure qutebrowser using the
# :set/:bind/:config-* commands without having to write a config.py
# file.
#
# Documentation:
#   qute://help/configuring.html
#   qute://help/settings.html

# Change the argument to True to still load settings configured via autoconfig.yml
config.load_autoconfig(False)

# Which cookies to accept. With QtWebEngine, this setting also controls
# other features with tracking capabilities similar to those of cookies;
# including IndexedDB, DOM storage, filesystem API, service workers, and
# AppCache. Note that with QtWebKit, only `all` and `never` are
# supported as per-domain values. Setting `no-3rdparty` or `no-
# unknown-3rdparty` per-domain on QtWebKit will have the same effect as
# `all`. If this setting is used with URL patterns, the pattern gets
# applied to the origin/first party URL of the page making the request,
# not the request URL. With QtWebEngine 5.15.0+, paths will be stripped
# from URLs, so URL patterns using paths will not match. With
# QtWebEngine 5.15.2+, subdomains are additionally stripped as well, so
# you will typically need to set this setting for `example.com` when the
# cookie is set on `somesubdomain.example.com` for it to work properly.
# To debug issues with this setting, start qutebrowser with `--debug
# --logfilter network --debug-flag log-cookies` which will show all
# cookies being set.
# Type: String
# Valid values:
#   - all: Accept all cookies.
#   - no-3rdparty: Accept cookies from the same origin only. This is known to break some sites, such as GMail.
#   - no-unknown-3rdparty: Accept cookies from the same origin only, unless a cookie is already set for the domain. On QtWebEngine, this is the same as no-3rdparty.
#   - never: Don't accept cookies at all.
config.set('content.cookies.accept', 'no-unknown-3rdparty', 'chrome-devtools://*')

# Which cookies to accept. With QtWebEngine, this setting also controls
# other features with tracking capabilities similar to those of cookies;
# including IndexedDB, DOM storage, filesystem API, service workers, and
# AppCache. Note that with QtWebKit, only `all` and `never` are
# supported as per-domain values. Setting `no-3rdparty` or `no-
# unknown-3rdparty` per-domain on QtWebKit will have the same effect as
# `all`. If this setting is used with URL patterns, the pattern gets
# applied to the origin/first party URL of the page making the request,
# not the request URL. With QtWebEngine 5.15.0+, paths will be stripped
# from URLs, so URL patterns using paths will not match. With
# QtWebEngine 5.15.2+, subdomains are additionally stripped as well, so
# you will typically need to set this setting for `example.com` when the
# cookie is set on `somesubdomain.example.com` for it to work properly.
# To debug issues with this setting, start qutebrowser with `--debug
# --logfilter network --debug-flag log-cookies` which will show all
# cookies being set.
# Type: String
# Valid values:
#   - all: Accept all cookies.
#   - no-3rdparty: Accept cookies from the same origin only. This is known to break some sites, such as GMail.
#   - no-unknown-3rdparty: Accept cookies from the same origin only, unless a cookie is already set for the domain. On QtWebEngine, this is the same as no-3rdparty.
#   - never: Don't accept cookies at all.
config.set('content.cookies.accept', 'no-unknown-3rdparty', 'devtools://*')

# Value to send in the `Accept-Language` header. Note that the value
# read from JavaScript is always the global value.
# Type: String
config.set('content.headers.accept_language', '', 'https://matchmaker.krunker.io/*')

# Load images automatically in web pages.
# Type: Bool
config.set('content.images', True, 'chrome-devtools://*')

# Load images automatically in web pages.
# Type: Bool
config.set('content.images', True, 'devtools://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'chrome-devtools://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'devtools://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'chrome://*/*')

c.content.javascript.clipboard = "access-paste"

# Allow websites to show notifications.
# Type: BoolAsk
# Valid values:
#   - true
#   - false
#   - ask
#config.set('content.notifications.enabled', False, 'https://www.reddit.com')

#config.set('content.images', False, '*://example.com/')

# search
c.url.searchengines = {
  'DEFAULT': 'https://farside.link/searxng/search?q={}',
  'eco': 'https://www.ecosia.org/search?method=index&q={}',
  'whoo': 'https://farside.link/whoogle/search?q={}&lang_interface=en',
  'ph': 'https://www.phind.com/search?q={}',
}

# Page(s) to open at the start.
# Type: List of FuzzyUrl, or FuzzyUrl
c.url.start_pages = '~/.config/qutebrowser/start/start.html'
c.url.default_page = '~/.config/qutebrowser/start/start.html'

# dark mode
# c.colors.webpage.darkmode.enabled = True
c.colors.webpage.preferred_color_scheme = "dark"
c.colors.webpage.darkmode.policy.images = "smart"
c.colors.webpage.darkmode.policy.page = "smart"

# fonts
c.fonts.default_family = ["JetBrainsMono NF", "Noto Color Emoji"]
c.fonts.web.family.standard = "ubuntunerdfont"
c.fonts.web.family.fixed = "jetbrainsmononerdfont"
c.fonts.web.family.serif = "gomononerdfont"

c.scrolling.smooth = False
c.scrolling.bar = "when-searching"

# custom bindings
# bindings = {
#   "<Ctrl-Shift-l>": "spawn --userscript bitwarden.py",
# }

# theme
config.source('qutewal.py')

c.tabs.position = "left"
c.tabs.show = "multiple"
c.tabs.last_close = "close"
c.tabs.width = 30

c.auto_save.session = True

c.content.default_encoding = "utf-8"
c.content.blocking.method = "both"

c.editor.command = ["emacsclient", "-c", "-a", "'nvim'", "{}"]

c.content.user_stylesheets = "~/.config/qutebrowser/pog.css"

config.bind('<ctrl+j>', 'completion-item-focus next', 'command')
config.bind('<ctrl+k>', 'completion-item-focus prev', 'command')
config.bind('<ctrl+shift+l>', 'spawn --userscript qute-keepassxc --key 9989A4EC', mode='insert')
config.bind('pw', 'spawn --userscript qute-keepassxc --key 9989A4EC', mode='normal')
config.bind('gr', 'config-source', mode='normal')
config.bind('cn', 'clear-messages', mode='normal')
