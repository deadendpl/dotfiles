# Change the argument to True to still load settings configured via autoconfig.yml
config.load_autoconfig(False)

config.set('content.cookies.accept', 'no-unknown-3rdparty', 'chrome-devtools://*')
config.set('content.cookies.accept', 'no-unknown-3rdparty', 'devtools://*')

# config.set('content.headers.accept_language', '', 'https://matchmaker.krunker.io/*')

config.set('content.images', True, 'chrome-devtools://*')
config.set('content.images', True, 'devtools://*')

config.set('content.javascript.enabled', True, 'chrome-devtools://*')
config.set('content.javascript.enabled', True, 'devtools://*')
config.set('content.javascript.enabled', True, 'chrome://*/*')

c.content.javascript.clipboard = "access-paste"

# search
c.url.searchengines = {
  'DEFAULT': 'https://search.brave.com/search?q={}',
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

c.editor.command = ["emacsclient", "-c", "-a", "''", "{}"]

c.content.user_stylesheets = "~/.config/qutebrowser/pog.css"

config.bind('<ctrl+j>', 'completion-item-focus next', 'command')
config.bind('<ctrl+k>', 'completion-item-focus prev', 'command')
config.bind('<ctrl+l>', 'command-accept', 'command')
config.bind('<ctrl+shift+l>', 'spawn --userscript qute-keepassxc --key B7917853', mode='insert')
config.bind('cn', 'clear-messages', mode='normal')
config.bind('gr', 'config-source', mode='normal')
config.bind('gp', 'cmd-set-text :open {clipboard}', mode='normal')
config.bind('gP', 'cmd-set-text :open -t {clipboard}', mode='normal')
config.bind('pw', 'spawn --userscript qute-keepassxc --key B7917853', mode='normal')
config.bind('pt', 'spawn --userscript qute-keepassxc --key B7917853 --totp', mode='normal')
config.bind('gG', 'spawn --userscript grease-toggle.sh', mode='normal')
