"use client"

import { useState, useEffect } from 'react'
import { useRouter } from 'next/navigation'
import { ArrowLeft, ArrowRight, Play } from 'lucide-react'
import { Button } from '@/components/ui/button'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { GameTypeSelector } from '@/components/GameTypeSelector'
import { PlayerSelector } from '@/components/PlayerSelector'
import { GameSettings } from '@/components/GameSettings'
import { LoadingScreen } from '@/components/LoadingScreen'
import { supabase, isSupabaseConfigured } from '@/lib/supabase'
import { useToast } from '@/components/ui/use-toast'
import type { GameType, CompletionType, Player } from '@/lib/types'

type Step = 'game' | 'players' | 'settings'

const steps: Step[] = ['game', 'players', 'settings']

export default function PlayPage() {
  const router = useRouter()
  const { toast } = useToast()
  const [currentStep, setCurrentStep] = useState<Step>('game')
  const [players, setPlayers] = useState<Player[]>([])
  const [loading, setLoading] = useState(true)
  const [creating, setCreating] = useState(false)

  // Form state
  const [gameType, setGameType] = useState<GameType | null>(null)
  const [selectedPlayerIds, setSelectedPlayerIds] = useState<string[]>([])
  const [completionType, setCompletionType] = useState<CompletionType>('rounds')
  const [maxRounds, setMaxRounds] = useState(12)
  const [maxPoints, setMaxPoints] = useState(200)

  useEffect(() => {
    if (!isSupabaseConfigured()) {
      setLoading(false)
      return
    }

    async function loadPlayers() {
      const { data, error } = await supabase
        .from('players')
        .select('*')
        .order('name')

      if (error) {
        console.error('Error loading players:', error)
      } else {
        setPlayers(data as Player[])
      }
      setLoading(false)
    }

    loadPlayers()
  }, [])

  const currentStepIndex = steps.indexOf(currentStep)

  const canProceed = () => {
    switch (currentStep) {
      case 'game':
        return gameType !== null
      case 'players':
        return selectedPlayerIds.length >= 2
      case 'settings':
        return true
      default:
        return false
    }
  }

  const goNext = () => {
    const nextIndex = currentStepIndex + 1
    if (nextIndex < steps.length) {
      setCurrentStep(steps[nextIndex])
    }
  }

  const goBack = () => {
    const prevIndex = currentStepIndex - 1
    if (prevIndex >= 0) {
      setCurrentStep(steps[prevIndex])
    }
  }

  const togglePlayer = (playerId: string) => {
    setSelectedPlayerIds((prev) =>
      prev.includes(playerId)
        ? prev.filter((id) => id !== playerId)
        : [...prev, playerId]
    )
  }

  const startGame = async () => {
    if (!gameType || selectedPlayerIds.length < 2) return

    setCreating(true)

    try {
      // Create the game
      const { data: game, error: gameError } = await supabase
        .from('games')
        .insert({
          game_type: gameType,
          completion_type: completionType,
          max_rounds: completionType === 'rounds' ? maxRounds : null,
          max_points: completionType === 'points' ? maxPoints : null,
          status: 'active',
        })
        .select()
        .single()

      if (gameError) throw gameError

      // Add players to the game
      const gamePlayers = selectedPlayerIds.map((playerId) => ({
        game_id: game.id,
        player_id: playerId,
        final_score: 0,
      }))

      const { error: playersError } = await supabase
        .from('game_players')
        .insert(gamePlayers)

      if (playersError) throw playersError

      toast({
        title: 'Game started!',
        description: 'Good luck and have fun!',
      })

      // Navigate to the game page
      router.push(`/game/${game.id}`)
    } catch (error) {
      console.error('Error creating game:', error)
      toast({
        title: 'Error',
        description: 'Failed to create game. Please try again.',
        variant: 'destructive',
      })
    } finally {
      setCreating(false)
    }
  }

  if (loading) {
    return <LoadingScreen message="Loading players..." />
  }

  return (
    <div className="max-w-2xl mx-auto space-y-6">
      {/* Progress indicator */}
      <div className="flex items-center justify-center gap-2">
        {steps.map((step, index) => (
          <div key={step} className="flex items-center">
            <div
              className={`w-8 h-8 rounded-full flex items-center justify-center text-sm font-semibold transition-colors ${
                index <= currentStepIndex
                  ? 'bg-gn-gold text-gn-licorice'
                  : 'bg-muted text-muted-foreground'
              }`}
            >
              {index + 1}
            </div>
            {index < steps.length - 1 && (
              <div
                className={`w-12 h-1 mx-1 rounded transition-colors ${
                  index < currentStepIndex ? 'bg-gn-gold' : 'bg-muted'
                }`}
              />
            )}
          </div>
        ))}
      </div>

      {/* Step content */}
      <Card>
        <CardHeader>
          <CardTitle className="text-center">
            {currentStep === 'game' && 'Choose a Game'}
            {currentStep === 'players' && 'Select Players'}
            {currentStep === 'settings' && 'Game Settings'}
          </CardTitle>
        </CardHeader>
        <CardContent>
          {currentStep === 'game' && (
            <GameTypeSelector
              selected={gameType}
              onSelect={setGameType}
            />
          )}

          {currentStep === 'players' && (
            <PlayerSelector
              players={players}
              selectedIds={selectedPlayerIds}
              onToggle={togglePlayer}
            />
          )}

          {currentStep === 'settings' && (
            <GameSettings
              completionType={completionType}
              onCompletionTypeChange={setCompletionType}
              maxRounds={maxRounds}
              onMaxRoundsChange={setMaxRounds}
              maxPoints={maxPoints}
              onMaxPointsChange={setMaxPoints}
            />
          )}
        </CardContent>
      </Card>

      {/* Navigation buttons */}
      <div className="flex justify-between">
        <Button
          variant="outline"
          onClick={goBack}
          disabled={currentStepIndex === 0}
        >
          <ArrowLeft className="h-4 w-4 mr-2" />
          Back
        </Button>

        {currentStep === 'settings' ? (
          <Button
            variant="gold"
            onClick={startGame}
            disabled={!canProceed() || creating}
          >
            {creating ? 'Creating...' : (
              <>
                <Play className="h-4 w-4 mr-2" />
                Start Game
              </>
            )}
          </Button>
        ) : (
          <Button
            variant="gold"
            onClick={goNext}
            disabled={!canProceed()}
          >
            Next
            <ArrowRight className="h-4 w-4 ml-2" />
          </Button>
        )}
      </div>
    </div>
  )
}
