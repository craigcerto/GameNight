import type { Config } from 'tailwindcss'

const config: Config = {
  darkMode: ["class"],
  content: [
    './src/pages/**/*.{js,ts,jsx,tsx,mdx}',
    './src/components/**/*.{js,ts,jsx,tsx,mdx}',
    './src/app/**/*.{js,ts,jsx,tsx,mdx}',
  ],
  theme: {
    extend: {
      colors: {
        // Modern Neon color palette
        'neon-pink': '#ff2d75',
        'neon-cyan': '#00f0ff',
        'neon-purple': '#9d4edd',
        'neon-yellow': '#f0e130',
        'neon-green': '#39ff14',

        // Legacy colors (keeping for compatibility)
        'gn-darkblue': '#586F7C',
        'gn-gold': '#ff2d75', // Now maps to neon pink
        'gn-licorice': '#0a0a0f',
        'gn-white': '#F4F4F9',
        'gn-cream': '#F4F4F9',
        'gn-purple': '#9d4edd',

        // Semantic colors
        border: "hsl(var(--border))",
        input: "hsl(var(--input))",
        ring: "hsl(var(--ring))",
        background: "hsl(var(--background))",
        foreground: "hsl(var(--foreground))",
        primary: {
          DEFAULT: "hsl(var(--primary))",
          foreground: "hsl(var(--primary-foreground))",
        },
        secondary: {
          DEFAULT: "hsl(var(--secondary))",
          foreground: "hsl(var(--secondary-foreground))",
        },
        destructive: {
          DEFAULT: "hsl(var(--destructive))",
          foreground: "hsl(var(--destructive-foreground))",
        },
        muted: {
          DEFAULT: "hsl(var(--muted))",
          foreground: "hsl(var(--muted-foreground))",
        },
        accent: {
          DEFAULT: "hsl(var(--accent))",
          foreground: "hsl(var(--accent-foreground))",
        },
        popover: {
          DEFAULT: "hsl(var(--popover))",
          foreground: "hsl(var(--popover-foreground))",
        },
        card: {
          DEFAULT: "hsl(var(--card))",
          foreground: "hsl(var(--card-foreground))",
        },
      },
      borderRadius: {
        lg: "var(--radius)",
        md: "calc(var(--radius) - 2px)",
        sm: "calc(var(--radius) - 4px)",
      },
      fontFamily: {
        sans: ['var(--font-inter)'],
        display: ['var(--font-display)'],
      },
      keyframes: {
        "accordion-down": {
          from: { height: "0" },
          to: { height: "var(--radix-accordion-content-height)" },
        },
        "accordion-up": {
          from: { height: "var(--radix-accordion-content-height)" },
          to: { height: "0" },
        },
        "neon-pulse": {
          "0%, 100%": {
            boxShadow: "0 0 5px #ff2d75, 0 0 10px #ff2d75, 0 0 15px #ff2d75"
          },
          "50%": {
            boxShadow: "0 0 10px #ff2d75, 0 0 20px #ff2d75, 0 0 30px #ff2d75, 0 0 40px #ff2d75"
          },
        },
        "glow": {
          "0%, 100%": {
            boxShadow: "0 0 5px #ff2d75, 0 0 10px #ff2d75, 0 0 15px #ff2d75"
          },
          "50%": {
            boxShadow: "0 0 10px #ff2d75, 0 0 20px #ff2d75, 0 0 30px #ff2d75"
          },
        },
        "pulse-neon": {
          "0%, 100%": { opacity: "1" },
          "50%": { opacity: "0.8" },
        },
        "float": {
          "0%, 100%": { transform: "translateY(0)" },
          "50%": { transform: "translateY(-5px)" },
        },
        "fadeIn": {
          "0%": { opacity: "0", transform: "translateY(-10px)" },
          "100%": { opacity: "1", transform: "translateY(0)" },
        },
      },
      animation: {
        "accordion-down": "accordion-down 0.2s ease-out",
        "accordion-up": "accordion-up 0.2s ease-out",
        "neon-pulse": "neon-pulse 2s ease-in-out infinite",
        "glow": "glow 2s ease-in-out infinite",
        "pulse-neon": "pulse-neon 2s ease-in-out infinite",
        "pulse-gold": "pulse-neon 2s ease-in-out infinite",
        "float": "float 3s ease-in-out infinite",
        "fadeIn": "fadeIn 0.3s ease-out",
      },
      boxShadow: {
        'neon-pink': '0 0 10px #ff2d75, 0 0 20px #ff2d75',
        'neon-cyan': '0 0 10px #00f0ff, 0 0 20px #00f0ff',
        'neon-purple': '0 0 10px #9d4edd, 0 0 20px #9d4edd',
      },
    },
  },
  plugins: [require("tailwindcss-animate")],
}

export default config
