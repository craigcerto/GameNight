"use client"

import { useEffect, useState } from 'react'
import Link from 'next/link'
import Image from 'next/image'
import { Play, Trophy, TrendingUp, Clock, Sparkles, ChevronRight } from 'lucide-react'
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
    <div className="space-y-10">
      {/* Hero section */}
      <section className="relative py-16 text-center">
        {/* Background glow effect */}
        <div className="absolute inset-0 overflow-hidden pointer-events-none">
          <div className="absolute top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 w-[600px] h-[400px] bg-[#ff2d75]/20 rounded-full blur-[120px]" />
          <div className="absolute top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 w-[400px] h-[300px] bg-[#00f0ff]/10 rounded-full blur-[100px]" />
        </div>

        <div className="relative z-10">
          {/* Logo */}
          <div className="flex justify-center mb-8">
            <div className="relative">
              <Image
                src="/images/neon.png"
                alt="GameNight"
                width={380}
                height={190}
                className="max-w-full h-auto"
                style={{
                  filter: 'drop-shadow(0 0 40px rgba(255, 45, 117, 0.5)) drop-shadow(0 0 80px rgba(255, 45, 117, 0.3))'
                }}
                priority
              />
            </div>
          </div>

          {/* Tagline */}
          <p className="text-xl text-white/50 mb-10 max-w-lg mx-auto font-light tracking-wide">
            Track scores for Dominoes, Rummy, Mahjong and more
          </p>

          {/* CTA Button */}
          <Link href="/play">
            <Button variant="gold" size="xl" className="glow-pulse group">
              <Play className="h-5 w-5 mr-2 transition-transform group-hover:scale-110" />
              Start New Game
              <Sparkles className="h-4 w-4 ml-2 opacity-70" />
            </Button>
          </Link>
        </div>
      </section>

      {/* Active Games Section */}
      {activeGames.length > 0 && (
        <section>
          <Card className="card-cyan overflow-hidden">
            <CardHeader className="border-b border-[#00f0ff]/20">
              <CardTitle className="flex items-center gap-3">
                <div className="relative">
                  <Clock className="h-5 w-5 text-[#00f0ff]" />
                  <div className="absolute inset-0 animate-ping">
                    <Clock className="h-5 w-5 text-[#00f0ff] opacity-40" />
                  </div>
                </div>
                <span className="text-[#00f0ff] font-semibold">Game In Progress</span>
              </CardTitle>
            </CardHeader>
            <CardContent className="p-4">
              <div className="space-y-3">
                {activeGames.map((game) => (
                  <Link key={game.id} href={`/game/${game.id}`}>
                    <div className="group flex items-center justify-between p-4 rounded-xl bg-black/30 border border-white/5 hover:border-[#00f0ff]/40 hover:bg-[#00f0ff]/5 transition-all duration-300 cursor-pointer">
                      <div className="flex items-center gap-4">
                        <div className="w-12 h-12 rounded-xl bg-white/5 flex items-center justify-center text-2xl">
                          {game.game_type === 'dominoes' && '🁣'}
                          {game.game_type === 'rummy' && '🃏'}
                          {game.game_type === 'mahjong' && '🀄'}
                        </div>
                        <div>
                          <p className="font-semibold text-white capitalize">{game.game_type}</p>
                          <p className="text-sm text-white/40">
                            {game.game_players.length} players
                          </p>
                        </div>
                      </div>
                      <Button variant="neon-cyan" size="sm" className="group-hover:glow-cyan">
                        Resume
                        <ChevronRight className="h-4 w-4 ml-1 transition-transform group-hover:translate-x-1" />
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
          <CardHeader className="border-b border-white/5">
            <CardTitle className="flex items-center gap-3">
              <div className="w-8 h-8 rounded-lg bg-[#ff2d75]/20 flex items-center justify-center">
                <TrendingUp className="h-4 w-4 text-[#ff2d75]" />
              </div>
              <span className="gradient-text font-semibold">Top Players</span>
            </CardTitle>
          </CardHeader>
          <CardContent className="p-4">
            {loading ? (
              <div className="flex items-center justify-center py-8">
                <div className="w-6 h-6 border-2 border-[#ff2d75] border-t-transparent rounded-full animate-spin" />
              </div>
            ) : topPlayers.length > 0 ? (
              <div className="space-y-2">
                {topPlayers.map((entry, index) => (
                  <div
                    key={entry.player.id}
                    className={`flex items-center gap-3 p-3 rounded-xl transition-all duration-300 ${
                      index === 0
                        ? 'bg-gradient-to-r from-[#ff2d75]/20 to-transparent border border-[#ff2d75]/20'
                        : 'hover:bg-white/5'
                    }`}
                  >
                    {/* Rank Badge */}
                    <div className={`w-7 h-7 rounded-full flex items-center justify-center text-xs font-bold ${
                      index === 0
                        ? 'bg-gradient-to-br from-[#ffd700] to-[#ff8c00] text-black shadow-[0_0_15px_rgba(255,215,0,0.5)]'
                        : index === 1
                        ? 'bg-gradient-to-br from-[#c0c0c0] to-[#808080] text-black'
                        : index === 2
                        ? 'bg-gradient-to-br from-[#cd7f32] to-[#8b4513] text-black'
                        : 'bg-white/10 text-white/60'
                    }`}>
                      {index + 1}
                    </div>

                    {/* Player Avatar */}
                    <div
                      className="w-9 h-9 rounded-full flex items-center justify-center text-white text-sm font-bold transition-all duration-300"
                      style={{
                        backgroundColor: entry.player.color,
                        boxShadow: index === 0 ? `0 0 20px ${entry.player.color}80` : undefined,
                      }}
                    >
                      {entry.player.name.charAt(0)}
                    </div>

                    {/* Name */}
                    <span className="font-medium flex-1 text-white/90">
                      {entry.player.name}
                    </span>

                    {/* Wins */}
                    <span className={`font-bold text-sm px-2 py-1 rounded-lg ${
                      index === 0
                        ? 'bg-[#ff2d75]/20 text-[#ff2d75]'
                        : 'text-white/50'
                    }`}>
                      {entry.wins} {entry.wins === 1 ? 'win' : 'wins'}
                    </span>
                  </div>
                ))}
              </div>
            ) : (
              <div className="text-center py-8">
                <Trophy className="h-10 w-10 mx-auto text-white/20 mb-3" />
                <p className="text-white/40 text-sm">
                  No games played yet
                </p>
              </div>
            )}
          </CardContent>
        </Card>

        {/* Recent Games */}
        <Card className="md:col-span-2">
          <CardHeader className="border-b border-white/5">
            <CardTitle className="flex items-center gap-3">
              <div className="w-8 h-8 rounded-lg bg-[#ffe135]/20 flex items-center justify-center">
                <Trophy className="h-4 w-4 text-[#ffe135]" />
              </div>
              <span className="text-white font-semibold">Recent Games</span>
            </CardTitle>
          </CardHeader>
          <CardContent className="p-4">
            {loading ? (
              <div className="flex items-center justify-center py-12">
                <div className="w-6 h-6 border-2 border-[#ff2d75] border-t-transparent rounded-full animate-spin" />
              </div>
            ) : games.length > 0 ? (
              <GameHistory games={games} />
            ) : (
              <div className="text-center py-12">
                <Play className="h-10 w-10 mx-auto text-white/20 mb-3" />
                <p className="text-white/40 text-sm mb-4">
                  No games completed yet
                </p>
                <Link href="/play">
                  <Button variant="outline" size="sm">
                    Start your first game
                  </Button>
                </Link>
              </div>
            )}
          </CardContent>
        </Card>
      </section>
    </div>
  )
}
