"use client"

import { Switch } from '@/components/ui/switch'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
import type { CompletionType } from '@/lib/types'

interface GameSettingsProps {
  completionType: CompletionType
  onCompletionTypeChange: (type: CompletionType) => void
  maxRounds: number
  onMaxRoundsChange: (rounds: number) => void
  maxPoints: number
  onMaxPointsChange: (points: number) => void
}

export function GameSettings({
  completionType,
  onCompletionTypeChange,
  maxRounds,
  onMaxRoundsChange,
  maxPoints,
  onMaxPointsChange,
}: GameSettingsProps) {
  return (
    <div className="space-y-6">
      {/* Win Condition Toggle */}
      <div className="flex items-center justify-center gap-4">
        <Label
          className={completionType === 'rounds' ? 'text-gn-gold font-semibold' : 'text-muted-foreground'}
        >
          Rounds
        </Label>
        <Switch
          checked={completionType === 'points'}
          onCheckedChange={(checked) =>
            onCompletionTypeChange(checked ? 'points' : 'rounds')
          }
        />
        <Label
          className={completionType === 'points' ? 'text-gn-gold font-semibold' : 'text-muted-foreground'}
        >
          Points
        </Label>
      </div>

      {/* Value Input */}
      <div className="flex flex-col items-center gap-2">
        <Label className="text-sm text-muted-foreground">
          {completionType === 'rounds' ? 'Number of Rounds' : 'Winning Score'}
        </Label>
        <Input
          type="number"
          min={1}
          value={completionType === 'rounds' ? maxRounds : maxPoints}
          onChange={(e) => {
            const value = parseInt(e.target.value) || 1
            if (completionType === 'rounds') {
              onMaxRoundsChange(value)
            } else {
              onMaxPointsChange(value)
            }
          }}
          className="w-24 text-center text-lg font-semibold"
        />
      </div>

      {/* Summary */}
      <p className="text-center text-sm text-muted-foreground">
        {completionType === 'rounds' ? (
          <>Game ends after <strong className="text-gn-gold">{maxRounds}</strong> rounds</>
        ) : (
          <>First player to <strong className="text-gn-gold">{maxPoints}</strong> points wins</>
        )}
      </p>
    </div>
  )
}
