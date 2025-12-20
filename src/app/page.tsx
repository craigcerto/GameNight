"use client"

import { useEffect, useState } from 'react'
import Link from 'next/link'
import Image from 'next/image'
import { Play, Trophy, TrendingUp, Clock } from 'lucide-react'
import { Button } from '@/components/ui/button'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { GameHistory } from '@/components/GameHistory'
import { supabase, isSupabaseConfigured } from '@/lib/supabase'
import type { GameWithPlayers, Player } from '@/lib/types'

export default function HomePage() {
  const [games, setGames] = useState<GameWithPlayers[]>([])
  const [activeGames, setActiveGames] = useState<GameWithPlayers[]>([])
  const [topPlayers, setTopPlayers] = useState<{ player: Player; wins: number }[]>([])
  const [loading, setLoading] = useState(true)
  const [configured, setConfigured] = useState(true)

  useEffect(() => {
    if (!isSupabaseConfigured()) {
      setConfigured(false)
      setLoading(false)
      return
    }

    async function loadData() {
      try {
        // Load recent games with players
        const { data: gamesData, error: gamesError } = await supabase
          .from('games')
          .select(`
            *,
            game_players (
              *,
              player:players (*)
            ),
            winner:players!games_winner_id_fkey (*)
          `)
          .eq('status', 'completed')
          .order('started_at', { ascending: false })
          .limit(10)

        if (gamesError) throw gamesError
        setGames(gamesData as unknown as GameWithPlayers[])

        // Load active games
        const { data: activeGamesData, error: activeError } = await supabase
          .from('games')
          .select(`
            *,
            game_players (
              *,
              player:players (*)
            )
          `)
          .eq('status', 'active')
          .order('started_at', { ascending: false })

        if (!activeError && activeGamesData) {
          setActiveGames(activeGamesData as unknown as GameWithPlayers[])
        }

        // Calculate top players by wins
        const { data: playersData } = await supabase
          .from('players')
          .select('*')

        const { data: winsData } = await supabase
          .from('games')
          .select('winner_id')
          .eq('status', 'completed')
          .not('winner_id', 'is', null)

        if (playersData && winsData) {
          const winCounts: Record<string, number> = {}
          winsData.forEach((g) => {
            if (g.winner_id) {
              winCounts[g.winner_id] = (winCounts[g.winner_id] || 0) + 1
            }
          })

          const ranked = playersData
            .map((p) => ({ player: p as Player, wins: winCounts[p.id] || 0 }))
            .filter((p) => p.wins > 0)
            .sort((a, b) => b.wins - a.wins)
            .slice(0, 5)

          setTopPlayers(ranked)
        }
      } catch (error) {
        console.error('Error loading data:', error)
      } finally {
        setLoading(false)
      }
    }

    loadData()
  }, [])

  if (!configured) {
    return (
      <div className="max-w-2xl mx-auto text-center py-12">
        <div className="mb-8">
          <Trophy className="h-16 w-16 mx-auto text-gn-gold mb-4" />
          <h1 className="font-display text-4xl font-bold text-gn-gold neon-text mb-4">
            GameNight
          </h1>
          <p className="text-xl text-muted-foreground">
            Score tracker for your game nights
          </p>
        </div>

        <Card className="border-yellow-500/50 bg-yellow-500/10">
          <CardContent className="p-6">
            <h2 className="text-lg font-semibold text-yellow-500 mb-2">
              Setup Required
            </h2>
            <p className="text-muted-foreground mb-4">
              To get started, you need to configure Supabase:
            </p>
            <ol className="text-left text-sm space-y-2 text-muted-foreground">
              <li>1. Create a free account at <a href="https://supabase.com" className="text-gn-gold underline">supabase.com</a></li>
              <li>2. Create a new project</li>
              <li>3. Run the SQL migrations from <code className="bg-muted px-1 rounded">supabase/migrations/</code></li>
              <li>4. Copy <code className="bg-muted px-1 rounded">.env.local.example</code> to <code className="bg-muted px-1 rounded">.env.local</code></li>
              <li>5. Add your Supabase URL and anon key to <code className="bg-muted px-1 rounded">.env.local</code></li>
              <li>6. Restart the dev server</li>
            </ol>
          </CardContent>
        </Card>
      </div>
    )
  }

  return (
    <div className="space-y-8">
      {/* Hero section with neon logo */}
      <div className="text-center py-12">
        <div className="flex justify-center mb-8">
          <Image
            src="/images/neon.png"
            alt="GameNight"
            width={320}
            height={160}
            className="max-w-full h-auto drop-shadow-[0_0_30px_rgba(255,45,117,0.5)]"
            priority
          />
        </div>
        <p className="text-xl text-muted-foreground mb-8 max-w-md mx-auto">
          Track scores for Dominoes, Rummy, Mahjong and more!
        </p>
        <Link href="/play">
          <Button variant="gold" size="xl" className="neon-glow">
            <Play className="h-5 w-5 mr-2" />
            Start New Game
          </Button>
        </Link>
      </div>

      {/* Active Games Section */}
      {activeGames.length > 0 && (
        <Card className="border-neon-cyan/50 bg-neon-cyan/5 shadow-[0_0_20px_rgba(0,240,255,0.15)]">
          <CardHeader>
            <CardTitle className="flex items-center gap-2 text-neon-cyan">
              <Clock className="h-5 w-5 animate-pulse" />
              Game In Progress
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-3">
              {activeGames.map((game) => (
                <Link key={game.id} href={`/game/${game.id}`}>
                  <div className="flex items-center justify-between p-4 rounded-xl bg-card/50 border border-white/10 hover:border-neon-cyan/50 hover:bg-neon-cyan/5 transition-all duration-300 cursor-pointer">
                    <div className="flex items-center gap-3">
                      <span className="text-2xl">
                        {game.game_type === 'dominoes' && '🁣'}
                        {game.game_type === 'rummy' && '🃏'}
                        {game.game_type === 'mahjong' && '🀄'}
                      </span>
                      <div>
                        <p className="font-semibold capitalize">{game.game_type}</p>
                        <p className="text-sm text-muted-foreground">
                          {game.game_players.length} players
                        </p>
                      </div>
                    </div>
                    <Button variant="neon-cyan" size="sm">
                      Resume
                    </Button>
                  </div>
                </Link>
              ))}
            </div>
          </CardContent>
        </Card>
      )}

      <div className="grid md:grid-cols-3 gap-6">
        {/* Top Players */}
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <TrendingUp className="h-5 w-5 text-neon-pink" />
              <span className="bg-gradient-to-r from-neon-pink to-neon-cyan bg-clip-text text-transparent">
                Top Players
              </span>
            </CardTitle>
          </CardHeader>
          <CardContent>
            {loading ? (
              <p className="text-muted-foreground">Loading...</p>
            ) : topPlayers.length > 0 ? (
              <div className="space-y-3">
                {topPlayers.map((entry, index) => (
                  <div
                    key={entry.player.id}
                    className={`flex items-center gap-3 p-2 rounded-lg transition-all duration-300 ${
                      index === 0 ? 'bg-neon-pink/10' : 'hover:bg-white/5'
                    }`}
                  >
                    <span className={`w-6 text-center font-bold ${
                      index === 0 ? 'text-neon-pink' : 'text-muted-foreground'
                    }`}>
                      #{index + 1}
                    </span>
                    <div
                      className="w-8 h-8 rounded-full border-2 flex items-center justify-center text-white text-sm font-bold"
                      style={{
                        borderColor: entry.player.color,
                        backgroundColor: entry.player.color,
                        boxShadow: index === 0 ? `0 0 10px ${entry.player.color}` : undefined,
                      }}
                    >
                      {entry.player.name.charAt(0)}
                    </div>
                    <span
                      className="font-medium flex-1"
                      style={{ color: entry.player.color }}
                    >
                      {entry.player.name}
                    </span>
                    <span className={`font-semibold ${
                      index === 0 ? 'text-neon-pink drop-shadow-[0_0_5px_#ff2d75]' : 'text-neon-cyan'
                    }`}>
                      {entry.wins} {entry.wins === 1 ? 'win' : 'wins'}
                    </span>
                  </div>
                ))}
              </div>
            ) : (
              <p className="text-muted-foreground text-sm">
                No games played yet
              </p>
            )}
          </CardContent>
        </Card>

        {/* Recent Games */}
        <Card className="md:col-span-2">
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <Trophy className="h-5 w-5 text-neon-yellow" />
              <span className="text-white">Recent Games</span>
            </CardTitle>
          </CardHeader>
          <CardContent>
            {loading ? (
              <p className="text-muted-foreground">Loading...</p>
            ) : (
              <GameHistory games={games} />
            )}
          </CardContent>
        </Card>
      </div>
    </div>
  )
}
