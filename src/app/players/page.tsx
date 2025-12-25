"use client"

import { useState, useEffect } from 'react'
import { Plus, Edit2, Trash2, Save, X } from 'lucide-react'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
} from '@/components/ui/dialog'
import { PlayerCard } from '@/components/PlayerCard'
import { LoadingScreen } from '@/components/LoadingScreen'
import { supabase, isSupabaseConfigured } from '@/lib/supabase'
import { useToast } from '@/components/ui/use-toast'
import type { Player } from '@/lib/types'

const colorOptions = [
  '#321D71', '#8C1A10', '#48752C', '#2854C5', '#964B00',
  '#000000', '#CEA8BC', '#7CA7D8', '#0E2787', '#C6A15B',
  '#E74C3C', '#9B59B6', '#1ABC9C', '#F39C12', '#586F7C',
]

export default function PlayersPage() {
  const { toast } = useToast()
  const [players, setPlayers] = useState<Player[]>([])
  const [loading, setLoading] = useState(true)
  const [isAddDialogOpen, setIsAddDialogOpen] = useState(false)
  const [editingPlayer, setEditingPlayer] = useState<Player | null>(null)

  // Form state
  const [name, setName] = useState('')
  const [nickname, setNickname] = useState('')
  const [color, setColor] = useState(colorOptions[0])

  useEffect(() => {
    if (!isSupabaseConfigured()) {
      setLoading(false)
      return
    }
    loadPlayers()
  }, [])

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

  const resetForm = () => {
    setName('')
    setNickname('')
    setColor(colorOptions[0])
    setEditingPlayer(null)
  }

  const openEditDialog = (player: Player) => {
    setEditingPlayer(player)
    setName(player.name)
    setNickname(player.nickname || '')
    setColor(player.color)
    setIsAddDialogOpen(true)
  }

  const handleSave = async () => {
    if (!name.trim()) {
      toast({
        title: 'Error',
        description: 'Player name is required',
        variant: 'destructive',
      })
      return
    }

    try {
      if (editingPlayer) {
        // Update existing player
        const { error } = await (supabase
          .from('players')
          .update as any)({
            name: name.trim(),
            nickname: nickname.trim() || null,
            color,
          })
          .eq('id', editingPlayer.id)

        if (error) throw error

        toast({
          title: 'Player updated',
          description: `${name} has been updated`,
        })
      } else {
        // Create new player
        const { error } = await (supabase.from('players').insert as any)({
          name: name.trim(),
          nickname: nickname.trim() || null,
          color,
        })

        if (error) throw error

        toast({
          title: 'Player added',
          description: `${name} has been added to the roster`,
        })
      }

      resetForm()
      setIsAddDialogOpen(false)
      loadPlayers()
    } catch (error) {
      console.error('Error saving player:', error)
      toast({
        title: 'Error',
        description: 'Failed to save player',
        variant: 'destructive',
      })
    }
  }

  const handleDelete = async (player: Player) => {
    if (!confirm(`Are you sure you want to delete ${player.name}?`)) {
      return
    }

    try {
      const { error } = await supabase
        .from('players')
        .delete()
        .eq('id', player.id)

      if (error) throw error

      toast({
        title: 'Player deleted',
        description: `${player.name} has been removed`,
      })

      loadPlayers()
    } catch (error) {
      console.error('Error deleting player:', error)
      toast({
        title: 'Error',
        description: 'Failed to delete player. They may have game history.',
        variant: 'destructive',
      })
    }
  }

  if (loading) {
    return <LoadingScreen message="Meeting the players..." action="waving" />
  }

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-2xl font-bold">Players</h1>
          <p className="text-muted-foreground">
            Manage your game night crew
          </p>
        </div>

        <Dialog open={isAddDialogOpen} onOpenChange={(open) => {
          setIsAddDialogOpen(open)
          if (!open) resetForm()
        }}>
          <DialogTrigger asChild>
            <Button variant="gold">
              <Plus className="h-4 w-4 mr-2" />
              Add Player
            </Button>
          </DialogTrigger>
          <DialogContent>
            <DialogHeader>
              <DialogTitle>
                {editingPlayer ? 'Edit Player' : 'Add New Player'}
              </DialogTitle>
              <DialogDescription>
                {editingPlayer
                  ? 'Update player information'
                  : 'Add a new player to your game night roster'}
              </DialogDescription>
            </DialogHeader>

            <div className="space-y-4 py-4">
              <div className="space-y-2">
                <Label htmlFor="name">Name</Label>
                <Input
                  id="name"
                  placeholder="Enter player name"
                  value={name}
                  onChange={(e) => setName(e.target.value)}
                />
              </div>

              <div className="space-y-2">
                <Label htmlFor="nickname">Nickname (optional)</Label>
                <Input
                  id="nickname"
                  placeholder="Enter nickname"
                  value={nickname}
                  onChange={(e) => setNickname(e.target.value)}
                />
              </div>

              <div className="space-y-2">
                <Label>Color</Label>
                <div className="flex flex-wrap gap-2">
                  {colorOptions.map((c) => (
                    <button
                      key={c}
                      type="button"
                      onClick={() => setColor(c)}
                      className={`w-8 h-8 rounded-full border-2 transition-transform ${
                        color === c
                          ? 'border-white scale-110'
                          : 'border-transparent hover:scale-105'
                      }`}
                      style={{ backgroundColor: c }}
                    />
                  ))}
                </div>
              </div>

              {/* Preview */}
              <div className="pt-4 border-t">
                <Label className="mb-2 block">Preview</Label>
                <div className="flex justify-center">
                  <PlayerCard
                    player={{
                      id: 'preview',
                      name: name || 'Player Name',
                      nickname: nickname || null,
                      color,
                      avatar_url: null,
                      created_at: '',
                    }}
                    size="lg"
                  />
                </div>
              </div>
            </div>

            <DialogFooter>
              <Button
                variant="outline"
                onClick={() => {
                  resetForm()
                  setIsAddDialogOpen(false)
                }}
              >
                <X className="h-4 w-4 mr-2" />
                Cancel
              </Button>
              <Button variant="gold" onClick={handleSave}>
                <Save className="h-4 w-4 mr-2" />
                {editingPlayer ? 'Update' : 'Add'} Player
              </Button>
            </DialogFooter>
          </DialogContent>
        </Dialog>
      </div>

      {/* Players grid */}
      {players.length === 0 ? (
        <Card>
          <CardContent className="py-12 text-center">
            <p className="text-muted-foreground mb-4">
              No players yet. Add your first player to get started!
            </p>
            <Button variant="gold" onClick={() => setIsAddDialogOpen(true)}>
              <Plus className="h-4 w-4 mr-2" />
              Add Player
            </Button>
          </CardContent>
        </Card>
      ) : (
        <div className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-5 gap-4">
          {players.map((player) => (
            <Card key={player.id} className="relative group">
              <CardContent className="p-4">
                <PlayerCard player={player} size="md" className="w-full" />

                {/* Action buttons */}
                <div className="absolute top-2 right-2 opacity-0 group-hover:opacity-100 transition-opacity flex gap-1">
                  <Button
                    variant="ghost"
                    size="icon"
                    className="h-8 w-8"
                    onClick={() => openEditDialog(player)}
                  >
                    <Edit2 className="h-4 w-4" />
                  </Button>
                  <Button
                    variant="ghost"
                    size="icon"
                    className="h-8 w-8 text-destructive hover:text-destructive"
                    onClick={() => handleDelete(player)}
                  >
                    <Trash2 className="h-4 w-4" />
                  </Button>
                </div>
              </CardContent>
            </Card>
          ))}
        </div>
      )}
    </div>
  )
}
