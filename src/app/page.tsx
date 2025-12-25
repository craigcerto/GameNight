"use client"

import { useEffect, useState } from 'react'
import Link from 'next/link'
import Image from 'next/image'
import { Play, Trophy, TrendingUp, Clock, ChevronRight, Trash2, Save } from 'lucide-react'
import { Button } from '@/components/ui/button'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
} from '@/components/ui/alert-dialog'
import { GameHistory } from '@/components/GameHistory'
import { supabase, isSupabaseConfigured } from '@/lib/supabase'
import { useToast } from '@/components/ui/use-toast'
import type { GameWithPlayers, Player } from '@/lib/types'

export default function HomePage() {
  const { toast } = useToast()
  const [games, setGames] = useState<GameWithPlayers[]>([])
  const [activeGames, setActiveGames] = useState<GameWithPlayers[]>([])
  const [topPlayers, setTopPlayers] = useState<{ player: Player; wins: number }[]>([])
  const [loading, setLoading] = useState(true)
  const [configured, setConfigured] = useState(true)
  const [gameToDiscard, setGameToDiscard] = useState<string | null>(null)
  const [gameToEnd, setGameToEnd] = useState<string | null>(null)

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
          winsData.forEach((g: any) => {
            if (g.winner_id) {
              winCounts[g.winner_id] = (winCounts[g.winner_id] || 0) + 1
            }
          })

          const ranked = playersData
            .map((p: any) => ({ player: p as Player, wins: winCounts[p.id] || 0 }))
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

  // Discard a game (mark as cancelled, don't save scores)
  const handleDiscardGame = async (gameId: string) => {
    try {
      const { error } = await (supabase
        .from('games')
        .update as any)({
          status: 'cancelled',
          ended_at: new Date().toISOString()
        })
        .eq('id', gameId)

      if (error) throw error

      toast({
        title: 'Game Discarded',
        description: 'The game has been cancelled',
      })

      // Refresh active games
      const { data: activeGamesData } = await supabase
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

      if (activeGamesData) {
        setActiveGames(activeGamesData as unknown as GameWithPlayers[])
      }
    } catch (error) {
      console.error('Error discarding game:', error)
      toast({
        title: 'Error',
        description: 'Failed to discard game',
        variant: 'destructive',
      })
    } finally {
      setGameToDiscard(null)
    }
  }

  // End a game early (save current state and determine winner)
  const handleEndGame = async (gameId: string) => {
    try {
      // Get scores for this game
      const { data: scoresData, error: scoresError } = await supabase
        .from('scores')
        .select('*')
        .eq('game_id', gameId)

      if (scoresError) throw scoresError

      // Get game players
      const game = activeGames.find(g => g.id === gameId)
      if (!game) return

      // Calculate totals
      const playerTotals = game.game_players.map((gp) => {
        const total = (scoresData || [])
          .filter((s: any) => s.player_id === gp.player_id)
          .reduce((sum: number, s: any) => sum + s.score, 0)
        return { playerId: gp.player_id, total }
      })

      // Find winner (highest score)
      const sorted = [...playerTotals].sort((a, b) => b.total - a.total)
      const winnerId = sorted[0]?.playerId || null

      // Update game as completed
      const { error: updateError } = await (supabase
        .from('games')
        .update as any)({
          status: 'completed',
          winner_id: winnerId,
          ended_at: new Date().toISOString(),
        })
        .eq('id', gameId)

      if (updateError) throw updateError

      // Update final scores for all players
      for (const pt of playerTotals) {
        await (supabase
          .from('game_players')
          .update as any)({ final_score: pt.total })
          .eq('game_id', gameId)
          .eq('player_id', pt.playerId)
      }

      toast({
        title: 'Game Ended',
        description: 'The game has been saved with current scores',
      })

      // Refresh both active games and completed games
      const { data: activeGamesData } = await supabase
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

      const { data: gamesData } = await supabase
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

      if (activeGamesData) {
        setActiveGames(activeGamesData as unknown as GameWithPlayers[])
      }
      if (gamesData) {
        setGames(gamesData as unknown as GameWithPlayers[])
      }
    } catch (error) {
      console.error('Error ending game:', error)
      toast({
        title: 'Error',
        description: 'Failed to end game',
        variant: 'destructive',
      })
    } finally {
      setGameToEnd(null)
    }
  }

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
      <section className="text-center">
        {/* Title */}
        <h1 className="font-display text-6xl font-bold gradient-text mb-4">
          GameNight
        </h1>

        {/* Tagline */}
        <p className="text-base text-white/60 mb-8">
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
                  <div key={game.id} className="flex items-center justify-between p-4 rounded-xl bg-white/5 border border-white/10">
                    <Link href={`/game/${game.id}`} className="flex items-center gap-4 flex-1 hover:opacity-80 transition-opacity">
                      <div className="relative w-11 h-11 rounded-lg overflow-hidden flex-shrink-0">
                        <Image
                          src={`/images/games/${game.game_type === 'dominoes' ? 'dominos' : game.game_type}.jpg`}
                          alt={game.game_type}
                          fill
                          className="object-cover"
                        />
                      </div>
                      <div>
                        <p className="font-semibold text-white capitalize">{game.game_type}</p>
                        <p className="text-sm text-white/50">
                          {game.game_players.length} players
                        </p>
                      </div>
                    </Link>
                    <div className="flex items-center gap-2">
                      <Button
                        variant="outline"
                        size="sm"
                        onClick={(e) => {
                          e.stopPropagation()
                          setGameToEnd(game.id)
                        }}
                        className="border-[#00e5ff]/40 hover:bg-[#00e5ff]/10"
                      >
                        <Save className="h-4 w-4 mr-1" />
                        End
                      </Button>
                      <Button
                        variant="outline"
                        size="sm"
                        onClick={(e) => {
                          e.stopPropagation()
                          setGameToDiscard(game.id)
                        }}
                        className="border-[#ff2d75]/40 hover:bg-[#ff2d75]/10"
                      >
                        <Trash2 className="h-4 w-4 mr-1" />
                        Discard
                      </Button>
                      <Link href={`/game/${game.id}`}>
                        <Button variant="neon-cyan" size="sm">
                          Resume
                          <ChevronRight className="h-4 w-4 ml-1" />
                        </Button>
                      </Link>
                    </div>
                  </div>
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
                    {entry.player.avatar_url ? (
                      <div className="w-8 h-8 rounded-full overflow-hidden flex-shrink-0">
                        <Image
                          src={entry.player.avatar_url}
                          alt={entry.player.name}
                          width={32}
                          height={32}
                          className="w-full h-full object-cover"
                          style={{ imageRendering: 'pixelated' }}
                        />
                      </div>
                    ) : (
                      <div
                        className="w-8 h-8 rounded-full flex items-center justify-center text-white text-sm font-bold"
                        style={{ backgroundColor: entry.player.color }}
                      >
                        {entry.player.name.charAt(0)}
                      </div>
                    )}

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

      {/* Discard Game Confirmation Dialog */}
      <AlertDialog open={gameToDiscard !== null} onOpenChange={(open: boolean) => !open && setGameToDiscard(null)}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>Discard Game?</AlertDialogTitle>
            <AlertDialogDescription>
              This will cancel the game without saving any scores. This action cannot be undone.
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel>Cancel</AlertDialogCancel>
            <AlertDialogAction
              onClick={() => gameToDiscard && handleDiscardGame(gameToDiscard)}
              className="bg-[#ff2d75] hover:bg-[#ff2d75]/90"
            >
              Discard Game
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>

      {/* End Game Confirmation Dialog */}
      <AlertDialog open={gameToEnd !== null} onOpenChange={(open: boolean) => !open && setGameToEnd(null)}>
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>End Game Early?</AlertDialogTitle>
            <AlertDialogDescription>
              This will save the game with current scores and determine a winner based on the highest total score.
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel>Cancel</AlertDialogCancel>
            <AlertDialogAction
              onClick={() => gameToEnd && handleEndGame(gameToEnd)}
              className="bg-[#00e5ff] hover:bg-[#00e5ff]/90 text-black"
            >
              End & Save Game
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>
    </div>
  )
}
