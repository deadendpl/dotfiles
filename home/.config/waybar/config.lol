{
  "layer": "top",
  "position": "top",
  "height": 24,
  "spacing": 4,
  "modules-left": [
    "wlr/workspaces",
    "wlr/taskbar"
  ],
  "modules-center": [
    "hyprland/window"
  ],
  "modules-right": [
    "tray",
    "pulseaudio",
    "backlight",
    "network",
    "battery"
    "clock"
  ],
  "wlr/taskbar": {
    "on-click": "activate",
    "on-click-middle": "close",
    "ignore-list": [
      "foot"
    ]
  },
  "wlr/workspaces": {
    "on-click": "activate",
    "on-scroll-up": "hyprctl dispatch workspace e-1",
    "on-scroll-down": "hyprctl dispatch workspace e+1"
  },
  "hyprland/window": {
    "max-length": 128
  },
  "clock": {
    "format": "{:%c}",
    "tooltip-format": "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>"
  },
  "tray": {
    "spacing": 4
  },
  "custom/weather": {
    "exec": "~/.config/waybar/wittr.sh",
    "return-type": "json",
    "format": "{}",
    "tooltip": true,
    "interval": 900
  },

  "backlight": {
    "device": "intel_backlight",
    "format": "{percent}% {icon}",
    "format-icons": ["", ""]
  },






    "keyboard-state": {
        "numlock": true,
        "capslock": true,
        "format": "{name} {icon}",
        "format-icons": {
            "locked": "",
            "unlocked": ""
        }
    },





"wlr/workspaces": {
  "format": "{icon}",
  "on-click": "activate",
  "format-icons": {
    "1": "",
    "2": "",
    "3": "",
    "4": "",
    "5": "",
    "urgent": "",
    "active": "",
    "default": ""
  },
  "sort-by-number": true
},



    "tray": {
        // "icon-size": 21,
        "spacing": 10
    },
    "cpu": {
        "format": "{usage}% ",
        "tooltip": false
    },
    "memory": {
        "format": "{}% "
    },
    "temperature": {
        // "thermal-zone": 2,
        // "hwmon-path": "/sys/class/hwmon/hwmon2/temp1_input",
        "critical-threshold": 80,
        // "format-critical": "{temperatureC}°C {icon}",
        "format": "{temperatureC}°C {icon}",
        "format-icons": ["", "", ""]
    },
    "battery": {
        "states": {
            // "good": 95,
            "warning": 30,
            "critical": 15
        },
        "format": "{capacity}% {icon}",
        "format-charging": "{capacity}% ",
        "format-plugged": "{capacity}% ",
        "format-alt": "{time} {icon}",
        // "format-good": "", // An empty format will hide the module
        // "format-full": "",
        "format-icons": ["", "", "", "", ""]
    },


    "network": {
        // "interface": "wlp2*", // (Optional) To force the use of this interface
        "format-wifi": "{essid} ({signalStrength}%) ",
        "format-ethernet": "{ipaddr}/{cidr} ",
        "tooltip-format": "{ifname} via {gwaddr} ",
        "format-linked": "{ifname} (No IP) ",
        "format-disconnected": "Disconnected ⚠",
        "format-alt": "{ifname}: {ipaddr}/{cidr}"
    },
    "pulseaudio": {
        // "scroll-step": 1, // %, can be a float
        "format": "{volume}% {icon} {format_source}",
        "format-bluetooth": "{volume}% {icon} {format_source}",
        "format-bluetooth-muted": " {icon} {format_source}",
        "format-muted": " {format_source}",
        "format-source": "{volume}% ",
        "format-source-muted": "",
        "format-icons": {
            "headphone": "",
            "hands-free": "",
            "headset": "",
            "phone": "",
            "portable": "",
            "car": "",
            "default": ["", "", ""]
        },
        "on-click": "pavucontrol"
    },
    "custom/media": {
        "format": "{icon} {}",
        "return-type": "json",
        "max-length": 40,
        "format-icons": {
            "spotify": "",
            "default": "🎜"
        },
        "escape": true,
        "exec": "$HOME/.config/waybar/mediaplayer.py 2> /dev/null" // Script in resources folder
        // "exec": "$HOME/.config/waybar/mediaplayer.py --player spotify 2> /dev/null" // Filter player based on name
    }
}

