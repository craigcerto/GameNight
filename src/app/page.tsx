"use client"

import { useEffect, useState } from 'react'
import Link from 'next/link'
import Image from 'next/image'
import { Play, Trophy, TrendingUp, Clock, ChevronRight } from 'lucide-react'
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
          <Trophy className="h-16 w-16 mx-auto text-[#ff2d75] mb-4" />
          <h1 className="font-display text-4xl font-bold gradient-text mb-4">
            GameNight
          </h1>
          <p className="text-xl text-white/60">
            Score tracker for your game nights
          </p>
        </div>

        <Card className="card-highlight">
          <CardContent className="p-6">
            <h2 className="text-lg font-semibold text-[#ffe135] mb-2">
              Setup Required
            </h2>
            <p className="text-white/60 mb-4">
              To get started, you need to configure Supabase:
            </p>
            <ol className="text-left text-sm space-y-2 text-white/50">
              <li>1. Create a free account at <a href="https://supabase.com" className="text-[#00f0ff] underline hover:text-[#00f0ff]/80">supabase.com</a></li>
              <li>2. Create a new project</li>
              <li>3. Run the SQL migrations from <code className="bg-white/10 px-2 py-0.5 rounded">supabase/migrations/</code></li>
              <li>4. Copy <code className="bg-white/10 px-2 py-0.5 rounded">.env.local.example</code> to <code className="bg-white/10 px-2 py-0.5 rounded">.env.local</code></li>
              <li>5. Add your Supabase URL and anon key</li>
              <li>6. Restart the dev server</li>
            </ol>
          </CardContent>
        </Card>
      </div>
    )
  }

  return (
    <div className="space-y-8">
      {/* Hero section - no blur effects for performance */}
      <section className="py-12 text-center">
        {/* Logo */}
        <div className="flex justify-center mb-6">
          <Image
            src="/images/neon.png"
            alt="GameNight"
            width={340}
            height={170}
            className="max-w-full h-auto"
            priority
          />
        </div>

        {/* Tagline */}
        <p className="text-lg text-white/60 mb-8 max-w-md mx-auto">
          Track scores for Dominoes, Rummy, Mahjong and more
        </p>

        {/* CTA Button */}
        <Link href="/play">
          <Button variant="gold" size="xl">
            <Play className="h-5 w-5 mr-2" />
            Start New Game
          </Button>
        </Link>
      </section>

      {/* Active Games Section */}
      {activeGames.length > 0 && (
        <section>
          <Card className="card-cyan">
            <CardHeader className="border-b border-[#00e5ff]/20">
              <CardTitle className="flex items-center gap-3">
                <Clock className="h-5 w-5 text-[#00e5ff]" />
                <span className="text-[#00e5ff] font-semibold">Game In Progress</span>
              </CardTitle>
            </CardHeader>
            <CardContent className="p-4">
              <div className="space-y-3">
                {activeGames.map((game) => (
                  <Link key={game.id} href={`/game/${game.id}`}>
                    <div className="flex items-center justify-between p-4 rounded-xl bg-white/5 border border-white/10 hover:border-[#00e5ff]/40 hover:bg-[#00e5ff]/10 transition-colors cursor-pointer">
                      <div className="flex items-center gap-4">
                        <div className="w-11 h-11 rounded-lg bg-white/10 flex items-center justify-center text-2xl">
                          {game.game_type === 'dominoes' && '🁣'}
                          {game.game_type === 'rummy' && '🃏'}
                          {game.game_type === 'mahjong' && '🀄'}
                        </div>
                        <div>
                          <p className="font-semibold text-white capitalize">{game.game_type}</p>
                          <p className="text-sm text-white/50">
                            {game.game_players.length} players
                          </p>
                        </div>
                      </div>
                      <Button variant="neon-cyan" size="sm">
                        Resume
                        <ChevronRight className="h-4 w-4 ml-1" />
                      </Button>
                    </div>
                  </Link>
                ))}
              </div>
            </CardContent>
          </Card>
        </section>
      )}

      {/* Stats Grid */}
      <section className="grid md:grid-cols-3 gap-6">
        {/* Top Players */}
        <Card>
          <CardHeader className="border-b border-white/10">
            <CardTitle className="flex items-center gap-3">
              <TrendingUp className="h-5 w-5 text-[#ff3a7f]" />
              <span className="text-[#ff3a7f] font-semibold">Top Players</span>
            </CardTitle>
          </CardHeader>
          <CardContent className="p-4">
            {loading ? (
              <div className="flex items-center justify-center py-8">
                <div className="w-5 h-5 border-2 border-[#ff3a7f] border-t-transparent rounded-full animate-spin" />
              </div>
            ) : topPlayers.length > 0 ? (
              <div className="space-y-2">
                {topPlayers.map((entry, index) => (
                  <div
                    key={entry.player.id}
                    className={`flex items-center gap-3 p-3 rounded-xl ${
                      index === 0 ? 'bg-[#ff3a7f]/15 border border-[#ff3a7f]/25' : ''
                    }`}
                  >
                    {/* Rank Badge */}
                    <div className={`w-7 h-7 rounded-full flex items-center justify-center text-xs font-bold ${
                      index === 0 ? 'rank-gold'
                        : index === 1 ? 'rank-silver'
                        : index === 2 ? 'rank-bronze'
                        : 'bg-white/10 text-white/60'
                    }`}>
                      {index + 1}
                    </div>

                    {/* Player Avatar */}
                    <div
                      className="w-8 h-8 rounded-full flex items-center justify-center text-white text-sm font-bold"
                      style={{ backgroundColor: entry.player.color }}
                    >
                      {entry.player.name.charAt(0)}
                    </div>

                    {/* Name */}
                    <span className="font-medium flex-1 text-white/90">
                      {entry.player.name}
                    </span>

                    {/* Wins */}
                    <span className={`font-bold text-sm ${
                      index === 0 ? 'text-[#ff3a7f]' : 'text-white/50'
                    }`}>
                      {entry.wins} {entry.wins === 1 ? 'win' : 'wins'}
                    </span>
                  </div>
                ))}
              </div>
            ) : (
              <div className="text-center py-8">
                <Trophy className="h-10 w-10 mx-auto text-white/20 mb-3" />
                <p className="text-white/50 text-sm">No games played yet</p>
              </div>
            )}
          </CardContent>
        </Card>

        {/* Recent Games */}
        <Card className="md:col-span-2">
          <CardHeader className="border-b border-white/10">
            <CardTitle className="flex items-center gap-3">
              <Trophy className="h-5 w-5 text-[#ffea00]" />
              <span className="text-white font-semibold">Recent Games</span>
            </CardTitle>
          </CardHeader>
          <CardContent className="p-4">
            {loading ? (
              <div className="flex items-center justify-center py-10">
                <div className="w-5 h-5 border-2 border-[#ff3a7f] border-t-transparent rounded-full animate-spin" />
              </div>
            ) : games.length > 0 ? (
              <GameHistory games={games} />
            ) : (
              <div className="text-center py-10">
                <Play className="h-10 w-10 mx-auto text-white/20 mb-3" />
                <p className="text-white/50 text-sm mb-4">No games completed yet</p>
                <Link href="/play">
                  <Button variant="outline" size="sm">Start your first game</Button>
                </Link>
              </div>
            )}
          </CardContent>
        </Card>
      </section>
    </div>
  )
}
