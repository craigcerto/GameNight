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
