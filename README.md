# GameNight

A modern, responsive score tracker for your game nights. Track scores for Dominoes, Rummy, Mahjong and more!

## Features

- **Multiple Games**: Support for Dominoes, Rummy, and Mahjong
- **Flexible Win Conditions**: Play by rounds or first-to-score
- **Real-time Scoreboard**: Live score updates during games
- **Player Management**: Add, edit, and manage your game night crew
- **Game History**: View past games and track who's winning overall
- **Mobile Responsive**: Works great on phones and tablets
- **Dark Theme**: Easy on the eyes during evening game sessions

## Tech Stack

- **Frontend**: Next.js 14 + React + TypeScript
- **Styling**: Tailwind CSS + shadcn/ui components
- **Database**: Supabase (PostgreSQL)
- **Hosting**: Vercel (recommended)

## Quick Start

### 1. Clone and Install

```bash
git clone <your-repo-url>
cd GameNight
npm install
```

### 2. Set Up Supabase

1. Create a free account at [supabase.com](https://supabase.com)
2. Create a new project
3. Go to the SQL Editor and run the migrations:
   - First run `supabase/migrations/001_initial_schema.sql`
   - Then run `supabase/migrations/002_seed_players.sql` (optional - adds initial players)

### 3. Configure Environment Variables

```bash
cp .env.local.example .env.local
```

Edit `.env.local` and add your Supabase credentials:
- Go to your Supabase project dashboard
- Navigate to Settings > API
- Copy the Project URL and anon public key

```env
NEXT_PUBLIC_SUPABASE_URL=https://your-project-id.supabase.co
NEXT_PUBLIC_SUPABASE_ANON_KEY=your-anon-key-here
```

### 4. Run the Development Server

```bash
npm run dev
```

Open [http://localhost:3000](http://localhost:3000) to see the app.

## Deployment

### Deploy to Vercel (Recommended - Free)

1. Push your code to GitHub
2. Go to [vercel.com](https://vercel.com) and sign in with GitHub
3. Import your repository
4. Add your environment variables in the Vercel dashboard
5. Deploy!

Your app will be live at `https://your-project.vercel.app`

### Custom Domain

1. In Vercel, go to your project settings > Domains
2. Add your custom domain
3. Update your DNS settings as instructed

## Project Structure

```
GameNight/
├── src/
│   ├── app/                    # Next.js App Router pages
│   │   ├── page.tsx            # Home page
│   │   ├── play/               # Game setup
│   │   ├── game/[id]/          # Active game scoreboard
│   │   └── players/            # Player management
│   ├── components/             # React components
│   │   ├── ui/                 # Base UI components (shadcn)
│   │   ├── Navigation.tsx
│   │   ├── PlayerCard.tsx
│   │   ├── Scoreboard.tsx
│   │   └── ...
│   └── lib/                    # Utilities and types
│       ├── supabase.ts         # Database client
│       ├── types.ts            # TypeScript types
│       └── utils.ts            # Helper functions
├── public/images/              # Static images
├── supabase/migrations/        # Database schema
└── package.json
```

## Database Schema

### Tables

- **players**: Player profiles (name, nickname, color, avatar)
- **games**: Game sessions (type, settings, status, winner)
- **game_players**: Players in each game with final scores
- **scores**: Individual round scores

### Migrations

Run these SQL files in order in the Supabase SQL Editor:

1. `001_initial_schema.sql` - Creates tables, indexes, and RLS policies
2. `002_seed_players.sql` - (Optional) Seeds initial player data

## Customization

### Adding New Game Types

1. Update the `GameType` type in `src/lib/types.ts`
2. Add the new type to the database constraint in `001_initial_schema.sql`
3. Add an icon in `public/images/games/`
4. Update `GameTypeSelector.tsx` with the new option

### Changing the Color Theme

Edit `tailwind.config.ts` to modify the custom colors:

```typescript
colors: {
  'gn-darkblue': '#586F7C',
  'gn-gold': '#C6A15B',
  'gn-licorice': '#231B1B',
  'gn-white': '#F4F4F9',
}
```

## Development

```bash
# Run development server
npm run dev

# Build for production
npm run build

# Start production server
npm start

# Lint code
npm run lint
```

## Migration from Old Shiny App

If you have existing game data in CSV format, you can migrate it:

1. The old data files are in `data/game_*_summary.csv`
2. Parse the CSVs and insert into Supabase using the SQL Editor or a script
3. Match player names to the new player IDs in the database

## Cost

This entire stack runs on free tiers:

| Service | Cost | Limits |
|---------|------|--------|
| Vercel | $0 | 100 GB bandwidth/month |
| Supabase | $0 | 500 MB database, 50k auth users |

Perfect for personal use!

## License

MIT


  Frank the Dealer - Sprite Sheet Prompt

  10-frame horizontal sprite sheet of a dog casino dealer named Frank performing a card dealing animation loop. Retro pixel art style, 1990s arcade game aesthetic.

  Subject: Frank is a mini golden doodle (sample picture attached) wearing a classic green casino dealer visor and black bow tie. Anthropomorphic pose standing upright
  behind a casino table.

  Layout: Horizontal sprite sheet, 10 equally-spaced frames from left to right, each frame 256x256 pixels. Clean separation between frames with solid
  black background (#000000).

  Animation Sequence:
  Frame 1: Ready pose - Frank standing still, cards in deck in left paw
  Frame 2-3: Right paw reaching toward deck
  Frame 4-5: Paw pulling card from deck with slight motion blur
  Frame 6-7: Card dealing motion, arm extending forward
  Frame 8-9: Arm retracting, card released
  Frame 10: Return to Frame 1 ready pose for seamless loop

  Composition: 3/4 view angle, slightly looking down at the table. Consistent character proportions across all frames. Camera locked, only Frank moves.

  Lighting: Neon casino lighting with cyan and magenta rim lights from overhead. Key light at 45° top-left. Warm golden glow from casino table felt. High
  contrast shadows.

  Style: Bold pixel art with clean black outlines, limited color palette (16 colors max): neon cyan (#00FFFF), hot pink (#FF10F0), electric purple
  (#8B00FF), golden yellow (#FFD700), forest green (dealer visor #228B22), deep black (#0A0A0A), warm highlights. 1990s arcade fighter aesthetic, similar
  to Street Fighter II character animations. Smooth gradients on neon edges only.

  Technical: Each frame perfectly centered and aligned. Consistent sprite dimensions. Image rendering optimized for pixelated display. No anti-aliasing
  blur between frames. Export as PNG with transparency.

  Atmosphere: Smoky underground casino vibe, subtle card suit symbols (♠♥♦♣) in dark background, professional dealer energy.