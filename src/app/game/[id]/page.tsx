"use client"

import { useState, useEffect, useCallback } from 'react'
import { useParams, useRouter } from 'next/navigation'
import Link from 'next/link'
import Image from 'next/image'
import { Trophy, Home, RotateCcw, StopCircle, Settings } from 'lucide-react'
import { Button } from '@/components/ui/button'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from '@/components/ui/dialog'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
import { Scoreboard } from '@/components/Scoreboard'
import { ScoreEntry } from '@/components/ScoreEntry'
import { LoadingScreen } from '@/components/LoadingScreen'
import { supabase } from '@/lib/supabase'
import { useToast } from '@/components/ui/use-toast'
import { useFrank } from '@/contexts/FrankContext'
import { detectScoreEvent, detectCloseGame } from '@/lib/frankStateMachine'
import { GAME_TYPE_LABELS } from '@/lib/types'
import type { Game, Player, Score, GamePlayer } from '@/lib/types'

interface GameData extends Game {
  game_players: (GamePlayer & { player: Player })[]
}

export default function GamePage() {
  const params = useParams()
  const router = useRouter()
  const { toast } = useToast()
  const { triggerEvent } = useFrank()
  const gameId = params.id as string

  const [game, setGame] = useState<GameData | null>(null)
  const [scores, setScores] = useState<Score[]>([])
  const [currentRound, setCurrentRound] = useState(1)
  const [loading, setLoading] = useState(true)
  const [showWinnerModal, setShowWinnerModal] = useState(false)
  const [showEndGameModal, setShowEndGameModal] = useState(false)
  const [showSettingsModal, setShowSettingsModal] = useState(false)
  const [winner, setWinner] = useState<Player | null>(null)
  const [editMaxRounds, setEditMaxRounds] = useState<number>(12)
  const [editMaxPoints, setEditMaxPoints] = useState<number>(200)

  const players = game?.game_players.map((gp) => gp.player) || []

  const loadGame = useCallback(async () => {
    try {
      // Load game with players
      const { data: gameData, error: gameError } = await supabase
        .from('games')
        .select(`
          *,
          game_players (
            *,
            player:players (*)
          )
        `)
        .eq('id', gameId)
        .single()

      if (gameError) throw gameError

      // Cast once and reuse
      const typedGameData = gameData as unknown as GameData
      setGame(typedGameData)

      // Initialize edit values
      setEditMaxRounds(typedGameData.max_rounds || 12)
      setEditMaxPoints(typedGameData.max_points || 200)

      // Load existing scores
      const { data: scoresData, error: scoresError } = await supabase
        .from('scores')
        .select('*')
        .eq('game_id', gameId)
        .order('round', { ascending: true })

      if (scoresError) throw scoresError
      const typedScores = scoresData as Score[]
      setScores(typedScores)

      // Calculate current round
      if (typedScores && typedScores.length > 0) {
        const maxRound = Math.max(...typedScores.map((s) => s.round))
        const roundComplete = typedScores.filter((s) => s.round === maxRound).length ===
          typedGameData.game_players.length
        setCurrentRound(roundComplete ? maxRound + 1 : maxRound)
      }
    } catch (error) {
      console.error('Error loading game:', error)
      toast({
        title: 'Error',
        description: 'Failed to load game',
        variant: 'destructive',
      })
    } finally {
      setLoading(false)
    }
  }, [gameId, toast])

  useEffect(() => {
    loadGame()

    // Subscribe to realtime score updates
    const channel = supabase
      .channel(`game-${gameId}`)
      .on(
        'postgres_changes',
        {
          event: '*',
          schema: 'public',
          table: 'scores',
          filter: `game_id=eq.${gameId}`,
        },
        () => {
          loadGame()
        }
      )
      .subscribe()

    return () => {
      supabase.removeChannel(channel)
    }
  }, [gameId, loadGame])

  // Update game settings function
  const handleUpdateSettings = async () => {
    if (!game) return

    try {
      const updates: { max_rounds?: number; max_points?: number } = {}

      if (game.completion_type === 'rounds') {
        updates.max_rounds = editMaxRounds
      } else {
        updates.max_points = editMaxPoints
      }

      const { error } = await (supabase
        .from('games')
        .update as any)(updates)
        .eq('id', gameId)

      if (error) throw error

      toast({
        title: 'Settings Updated',
        description: game.completion_type === 'rounds'
          ? `Max rounds changed to ${editMaxRounds}`
          : `Score limit changed to ${editMaxPoints}`,
      })

      setShowSettingsModal(false)
      loadGame()
    } catch (error) {
      console.error('Error updating settings:', error)
      toast({
        title: 'Error',
        description: 'Failed to update game settings',
        variant: 'destructive',
      })
    }
  }

  // End game early function
  const handleEndGameEarly = async () => {
    if (!game) return

    try {
      // Calculate current totals
      const playerTotals = players.map((player) => {
        const total = scores
          .filter((s) => s.player_id === player.id)
          .reduce((sum, s) => sum + s.score, 0)
        return { player, total }
      })

      // Find winner (highest score)
      const sorted = [...playerTotals].sort((a, b) => b.total - a.total)
      const gameWinner = sorted[0]?.player || null

      // Update game as completed
      await (supabase
        .from('games')
        .update as any)({
          status: 'completed',
          winner_id: gameWinner?.id || null,
          ended_at: new Date().toISOString(),
        })
        .eq('id', gameId)

      // Update final scores for all players
      for (const pt of playerTotals) {
        await (supabase
          .from('game_players')
          .update as any)({ final_score: pt.total })
          .eq('game_id', gameId)
          .eq('player_id', pt.player.id)
      }

      setShowEndGameModal(false)

      if (gameWinner) {
        setWinner(gameWinner)
        setShowWinnerModal(true)
      } else {
        router.push('/')
      }

      loadGame()
    } catch (error) {
      console.error('Error ending game:', error)
      toast({
        title: 'Error',
        description: 'Failed to end game',
        variant: 'destructive',
      })
    }
  }

  const handleSubmitScores = async (
    roundScores: { playerId: string; score: number }[]
  ) => {
    if (!game) return

    try {
      // Insert scores for this round
      const scoreInserts = roundScores.map((s) => ({
        game_id: gameId,
        player_id: s.playerId,
        round: currentRound,
        score: s.score,
      }))

      const { error: insertError } = await (supabase
        .from('scores')
        .insert as any)(scoreInserts)

      if (insertError) throw insertError

      // Trigger round end drumroll
      triggerEvent('round_end')

      // Calculate new totals
      const allScores = [...scores, ...scoreInserts.map((s, i) => ({
        ...s,
        id: `temp-${i}`,
        created_at: new Date().toISOString(),
      }))]

      const playerTotals = players.map((player) => {
        const total = allScores
          .filter((s) => s.player_id === player.id)
          .reduce((sum, s) => sum + s.score, 0)
        return { player, total }
      })

      // Detect score-based events for this round
      roundScores.forEach((rs) => {
        const player = players.find((p) => p.id === rs.playerId)
        if (!player) return

        const playerScoresHistory = allScores
          .filter((s) => s.player_id === rs.playerId)
          .map((s) => s.score)
        const lastThreeScores = playerScoresHistory.slice(-3)

        const scoreEvent = detectScoreEvent(rs.score, {
          playerName: player.name,
          score: rs.score,
          lastThreeScores,
        })

        if (scoreEvent) {
          triggerEvent(scoreEvent, {
            playerName: player.name,
            score: rs.score,
          })
        }
      })

      // Check for close game
      const sorted = [...playerTotals].sort((a, b) => b.total - a.total)
      if (sorted.length >= 2) {
        const closeGameEvent = detectCloseGame(sorted[0].total, sorted[1].total)
        if (closeGameEvent) {
          triggerEvent(closeGameEvent, {
            scoreDiff: sorted[0].total - sorted[1].total,
          })
        }
      }

      // Check win conditions
      let gameWinner: Player | null = null
      let gameEnded = false

      if (game.completion_type === 'rounds' && currentRound >= (game.max_rounds || 12)) {
        // Rounds-based: highest score after all rounds
        gameEnded = true
        const sorted = playerTotals.sort((a, b) => b.total - a.total)
        gameWinner = sorted[0].player
      } else if (game.completion_type === 'points') {
        // Points-based: first to reach target
        const winner = playerTotals.find((pt) => pt.total >= (game.max_points || 200))
        if (winner) {
          gameEnded = true
          gameWinner = winner.player
        }
      }

      if (gameEnded && gameWinner) {
        // Trigger game win event
        triggerEvent('game_win', {
          playerName: gameWinner.name,
          isWinner: true,
          totalScore: playerTotals.find((pt) => pt.player.id === gameWinner.id)?.total,
        })

        // Update game as completed
        await (supabase
          .from('games')
          .update as any)({
            status: 'completed',
            winner_id: gameWinner.id,
            ended_at: new Date().toISOString(),
          })
          .eq('id', gameId)

        // Update final scores for all players
        for (const pt of playerTotals) {
          await (supabase
            .from('game_players')
            .update as any)({ final_score: pt.total })
            .eq('game_id', gameId)
            .eq('player_id', pt.player.id)
        }

        setWinner(gameWinner)
        setShowWinnerModal(true)
      } else {
        setCurrentRound((prev) => prev + 1)
        toast({
          title: `Round ${currentRound} complete!`,
          description: 'Enter scores for the next round',
        })
      }

      // Refresh scores
      loadGame()
    } catch (error) {
      console.error('Error submitting scores:', error)
      toast({
        title: 'Error',
        description: 'Failed to save scores',
        variant: 'destructive',
      })
    }
  }

  if (loading) {
    return <LoadingScreen message="Dealing the cards..." action="shuffling" />
  }

  if (!game) {
    return (
      <div className="text-center py-12">
        <p className="text-muted-foreground mb-4">Game not found</p>
        <Link href="/">
          <Button variant="outline">
            <Home className="h-4 w-4 mr-2" />
            Go Home
          </Button>
        </Link>
      </div>
    )
  }

  const isCompleted = game.status === 'completed'

  return (
    <div className="space-y-6">
      {/* Game header */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="font-display text-2xl font-bold flex items-center gap-3">
            <div className="relative w-10 h-10 rounded overflow-hidden flex-shrink-0">
              <Image
                src={`/images/games/${game.game_type === 'dominoes' ? 'dominos' : game.game_type}.jpg`}
                alt={GAME_TYPE_LABELS[game.game_type]}
                fill
                className="object-cover"
              />
            </div>
            {GAME_TYPE_LABELS[game.game_type]}
          </h1>
          <p className="text-muted-foreground">
            {game.completion_type === 'rounds'
              ? `Round ${Math.min(currentRound, game.max_rounds || 12)} of ${game.max_rounds}`
              : `First to ${game.max_points} points`}
          </p>
        </div>

        <div className="flex gap-2">
          {!isCompleted && (
            <>
              <Button
                variant="outline"
                size="sm"
                onClick={() => setShowSettingsModal(true)}
                className="text-[#00e5ff] border-[#00e5ff]/50 hover:bg-[#00e5ff]/10"
              >
                <Settings className="h-4 w-4 mr-2" />
                Settings
              </Button>
              <Button
                variant="outline"
                size="sm"
                onClick={() => setShowEndGameModal(true)}
                className="text-red-400 border-red-400/50 hover:bg-red-400/10"
              >
                <StopCircle className="h-4 w-4 mr-2" />
                End Game
              </Button>
            </>
          )}
          <Link href="/">
            <Button variant="outline" size="sm">
              <Home className="h-4 w-4 mr-2" />
              Home
            </Button>
          </Link>
        </div>
      </div>

      <div className="grid md:grid-cols-2 gap-6">
        {/* Score Entry */}
        <div>
          {isCompleted ? (
            <Card>
              <CardHeader>
                <CardTitle className="text-center text-gn-gold">
                  Game Complete!
                </CardTitle>
              </CardHeader>
              <CardContent className="text-center">
                <Trophy className="h-16 w-16 mx-auto text-gn-gold mb-4" />
                <p className="text-lg">
                  Winner:{' '}
                  <span
                    className="font-bold"
                    style={{ color: game.game_players.find(
                      (gp) => gp.player_id === game.winner_id
                    )?.player.color }}
                  >
                    {game.game_players.find(
                      (gp) => gp.player_id === game.winner_id
                    )?.player.name}
                  </span>
                </p>
                <Link href="/play" className="mt-4 inline-block">
                  <Button variant="gold">
                    <RotateCcw className="h-4 w-4 mr-2" />
                    Play Again
                  </Button>
                </Link>
              </CardContent>
            </Card>
          ) : (
            <ScoreEntry
              players={players}
              round={currentRound}
              onSubmit={handleSubmitScores}
            />
          )}
        </div>

        {/* Scoreboard */}
        <Scoreboard
          players={players}
          scores={scores}
          currentRound={currentRound - 1}
        />
      </div>

      {/* Score history table */}
      {scores.length > 0 && (
        <Card>
          <CardHeader>
            <CardTitle>Score History</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="overflow-x-auto">
              <table className="w-full text-sm">
                <thead>
                  <tr className="border-b">
                    <th className="text-left py-2 px-3">Round</th>
                    {players.map((player) => (
                      <th
                        key={player.id}
                        className="text-center py-2 px-3"
                        style={{ color: player.color }}
                      >
                        {player.name}
                      </th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {Array.from({ length: currentRound - 1 }, (_, i) => i + 1).map(
                    (round) => (
                      <tr key={round} className="border-b border-border/50">
                        <td className="py-2 px-3 font-medium">{round}</td>
                        {players.map((player) => {
                          const score = scores.find(
                            (s) => s.round === round && s.player_id === player.id
                          )
                          return (
                            <td
                              key={player.id}
                              className="text-center py-2 px-3"
                            >
                              {score?.score ?? '-'}
                            </td>
                          )
                        })}
                      </tr>
                    )
                  )}
                  {/* Totals row */}
                  <tr className="bg-muted/50 font-bold">
                    <td className="py-2 px-3">Total</td>
                    {players.map((player) => {
                      const total = scores
                        .filter((s) => s.player_id === player.id)
                        .reduce((sum, s) => sum + s.score, 0)
                      return (
                        <td
                          key={player.id}
                          className="text-center py-2 px-3 text-gn-gold"
                        >
                          {total}
                        </td>
                      )
                    })}
                  </tr>
                </tbody>
              </table>
            </div>
          </CardContent>
        </Card>
      )}

      {/* Game Settings Modal */}
      <Dialog open={showSettingsModal} onOpenChange={setShowSettingsModal}>
        <DialogContent>
          <DialogHeader>
            <DialogTitle>Game Settings</DialogTitle>
            <DialogDescription>
              Modify the game completion conditions mid-game.
            </DialogDescription>
          </DialogHeader>
          <div className="space-y-4 py-4">
            {game?.completion_type === 'rounds' ? (
              <div className="space-y-2">
                <Label htmlFor="maxRounds">Maximum Rounds</Label>
                <Input
                  id="maxRounds"
                  type="number"
                  min="1"
                  max="50"
                  value={editMaxRounds}
                  onChange={(e) => setEditMaxRounds(Number(e.target.value))}
                  className="w-full"
                />
                <p className="text-xs text-white/50">
                  Current: Round {Math.min(currentRound, game.max_rounds || 12)} of {game.max_rounds}
                </p>
              </div>
            ) : (
              <div className="space-y-2">
                <Label htmlFor="maxPoints">Score Limit</Label>
                <Input
                  id="maxPoints"
                  type="number"
                  min="10"
                  max="10000"
                  step="10"
                  value={editMaxPoints}
                  onChange={(e) => setEditMaxPoints(Number(e.target.value))}
                  className="w-full"
                />
                <p className="text-xs text-white/50">
                  Current target: {game?.max_points} points
                </p>
              </div>
            )}
          </div>
          <DialogFooter className="flex gap-3 sm:justify-end">
            <Button
              variant="outline"
              onClick={() => setShowSettingsModal(false)}
            >
              Cancel
            </Button>
            <Button
              variant="default"
              onClick={handleUpdateSettings}
              className="bg-[#00e5ff] hover:bg-[#00e5ff]/90 text-black"
            >
              <Settings className="h-4 w-4 mr-2" />
              Save Settings
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>

      {/* End Game Confirmation Modal */}
      <Dialog open={showEndGameModal} onOpenChange={setShowEndGameModal}>
        <DialogContent>
          <DialogHeader>
            <DialogTitle>End Game Early?</DialogTitle>
            <DialogDescription>
              Are you sure you want to end this game? The player with the highest
              score will be declared the winner.
            </DialogDescription>
          </DialogHeader>
          <DialogFooter className="flex gap-3 sm:justify-end">
            <Button
              variant="outline"
              onClick={() => setShowEndGameModal(false)}
            >
              Cancel
            </Button>
            <Button
              variant="destructive"
              onClick={handleEndGameEarly}
            >
              <StopCircle className="h-4 w-4 mr-2" />
              End Game
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>

      {/* Winner modal */}
      <Dialog open={showWinnerModal} onOpenChange={setShowWinnerModal}>
        <DialogContent className="text-center">
          <DialogHeader>
            <DialogTitle className="text-2xl">
              <Trophy className="h-12 w-12 mx-auto text-gn-gold mb-2" />
              Game Over!
            </DialogTitle>
            <DialogDescription className="text-lg">
              {winner && (
                <>
                  <span
                    className="font-bold text-xl"
                    style={{ color: winner.color }}
                  >
                    {winner.name}
                  </span>{' '}
                  wins the game!
                </>
              )}
            </DialogDescription>
          </DialogHeader>
          <DialogFooter className="flex justify-center gap-3 sm:justify-center">
            <Link href="/">
              <Button variant="outline">
                <Home className="h-4 w-4 mr-2" />
                Home
              </Button>
            </Link>
            <Link href="/play">
              <Button variant="gold">
                <RotateCcw className="h-4 w-4 mr-2" />
                Play Again
              </Button>
            </Link>
          </DialogFooter>
        </DialogContent>
      </Dialog>
    </div>
  )
}
